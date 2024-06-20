open Ast
open Context
open Structures.Vec
open Structures.Hashmap
open Middle.Def_id
open Middle.Ty
open Ir.Inst
open Middle.Ctx

let rec lower_block (lcx : lcx) block =
  let tcx = lcx#tcx in
  let expr_ty expr = tcx#get_def (local_def_id expr.expr_id) in
  let rec lower_block' () =
    let f stmt =
      match stmt with
      | Binding { binding_expr; binding_span = span; binding_id; _ } ->
          let did = local_def_id binding_id in
          let decision = tcx#get_decision_tree did in
          let v = lower binding_expr in
          let ty = tcx#get_def (local_def_id binding_id) in
          let exprs = new vec in
          exprs#push binding_expr;
          ignore @@ lower_match ~lett:true ty v decision exprs span
      | Assign (left, right) ->
          let dst = lower_lvalue left in
          let src = lower right in
          lcx#bx#store src dst left.expr_span
      | Stmt expr | Expr expr -> ignore (lower expr)
      | Assert (expr, msg) ->
          let open Ir in
          let cond = lower expr in
          let true_bb = Basicblock.create () in
          let false_bb = Basicblock.create () in
          let join_bb = Basicblock.create () in
          lcx#bx#br cond (Label true_bb) (Label false_bb);
          lcx#append_block_with_builder true_bb;
          lcx#bx#jmp (Label join_bb);
          lcx#append_block_with_builder false_bb;
          let loc =
            tcx#sess.parse_sess.sm#span_to_string expr.expr_span.lo
          in
          let msg =
            match msg with
            | Some msg ->
                (match msg.expr_kind with
                 | Lit (LitStr msg) ->
                     "  assertion failed with `" ^ msg ^ "` at "
                 | _ -> assert false)
            | None -> "  assertion failed at "
          in
          let msg = msg ^ loc ^ "\n" in
          lcx#bx#trap msg expr.expr_span;
          lcx#bx#jmp (Label join_bb);
          lcx#append_block_with_builder join_bb
    in
    block.block_stmts#iter f;
    match block.last_expr with Some expr -> lower expr | None -> lcx#bx#nop
  and lower_lit lit ty =
    match lit with
    | LitInt value -> lcx#bx#const_int ty value
    | LitFloat value -> lcx#bx#const_float ty value
    | LitStr value -> lcx#bx#const_string ty value
    | LitBool value -> lcx#bx#const_bool ty value
  and lower_autoderef expr =
    let ty = expr_ty expr in
    match !ty with
    | Ptr (_, ty) | Ref (_, ty) -> lower expr, ty
    | _ -> lower_lvalue expr, ty
  and lower_field expr ident =
    let ptr, ty = lower_autoderef expr in
    let (Variant variant) = tcx#non_enum_variant ty |> Option.get in
    let index = ref (-1) in
    find
      (fun (Middle.Ty.Field { name; ty }) ->
        incr index;
        if name = ident then Some (!index, ty) else None)
      variant.fields
    |> function
    | Some (index, ty') -> lcx#bx#gep ty ty' ptr index expr.expr_span
    | None -> assert false
  and lower_method e expr seg args =
    let name = (seg : path_segment).ident in
    let subst =
      match seg.args with
      | Some args ->
          map args (fun arg -> Middle.Ty.Ty (tcx#ast_ty_to_ty arg))
      | _ -> new vec
    in
    let first, ty = lower_autoderef expr in
    let method', _ = tcx#lookup_method ty name in
    let method' = SubstFolder.fold_ty tcx method' subst in
    let method' =
      Option.fold
        ~none:method'
        ~some:(fun subst -> SubstFolder.fold_ty tcx method' subst)
        (tcx#get_subst ty)
    in
    let first, ty =
      let open Middle.Ty in
      match Fn.args tcx method' with
      | args when args#empty -> assert false
      | args ->
          (match !(args#get 0) with
           | Ref (_, ty) -> first, ty
           | _ -> lcx#bx#move first expr.expr_span, ty)
    in
    let id, _ = tcx#lookup_method_def_id ty name |> Option.get in
    let subst = Subst (tcx#get_subst method' |> Option.get) in
    let instance = { def = Fn id; subst } in
    let fn = Global (Fn instance) in
    let args' = new vec in
    args'#push first;
    args'#append @@ map args (fun arg -> lower arg);
    lcx#bx#call method' fn args' e.expr_span
  and lower_lvalue e =
    let v = lower_lvalue' e in
    tcx#get_adjustment e.expr_id
    |> Option.map (fun adjust ->
           let open Adjustment in
           let ArrayToSlice = adjust.kind in
           let ptr = lcx#bx#alloca adjust.target e.expr_span in
           let v = lcx#bx#move v e.expr_span in
           let v =
             lcx#bx#coercion ArrayToSlice v adjust.target e.expr_span
           in
           lcx#bx#store v ptr e.expr_span;
           ptr)
    |> Option.value ~default:v
  and lower_lvalue' e =
    let ty = expr_ty e in
    match e.expr_kind with
    | Lit lit ->
        let ptr = lcx#bx#alloca ty e.expr_span in
        lcx#bx#store (lower_lit lit ty) ptr e.expr_span;
        ptr
    | Path path ->
        let res = tcx#res_map#unsafe_get path.path_id in
        (match res with
         | Local (_, id) -> lcx#locals#unsafe_get id
         | Def (id, (Fn | AssocFn)) ->
             let subst = Fn.subst ty in
             let instance = { def = Fn id; subst = Subst subst } in
             Global (Fn instance)
         | Def (id, Cons) ->
             let ptr = lcx#bx#alloca ty e.expr_span in
             let v = lower_variant ty id in
             lcx#bx#store v ptr e.expr_span;
             ptr
         | Def (id, Intrinsic) ->
             let subst = Fn.subst ty in
             let instance = { def = Intrinsic id; subst = Subst subst } in
             Global (Fn instance)
         | _ -> assert false)
    | Deref expr -> lower expr
    | Field (expr, ident) -> lower_field expr ident
    | MethodCall (expr, seg, args) -> lower_method e expr seg args
    | Slice exprs ->
        let values = map exprs lower in
        lcx#bx#aggregate (Slice ty) values e.expr_span
    | _ ->
        print_endline @@ tcx#sess.parse_sess.sm#span_to_string e.expr_span.lo;
        assert false
  and lower_variant ty id =
    let subst = tcx#get_subst ty |> Option.get in
    let key = tcx#def_key id in
    let parenid = Option.get key.parent in
    let adt = tcx#adt parenid (Subst subst) in
    let variants = tcx#variants adt |> Option.get in
    let vidx = ref @@ -1 in
    for i = 0 to variants#len - 1 do
      let (Variant variant) = variants#get i in
      if variant.def_id = id then vidx := i
    done;
    assert (!vidx >= 0);
    lcx#bx#aggregate
      (Adt (parenid, !vidx, Subst subst))
      (new vec)
      Source.Span.dummy
  and lower_match ?(lett = false) ty v decision arms span =
    let open Middle.Decision in
    let open Ir in
    let rec fold_vars f = function
      | Success body -> body.bindings#iter (fun _ var -> f var)
      | Switch (var, cases, default) ->
          f var;
          cases#iter (fun case ->
              case.args#iter f;
              fold_vars f case.body);
          Option.iter (fold_vars f) default
      | _ -> assert false
    in
    let vars = new hashmap in
    let ptrs = new hashmap in
    let bbs = new hashmap in
    fold_vars
      (fun var ->
        match vars#has var.index with
        | false ->
            let ptr = lcx#bx#alloca var.ty span in
            assert (vars#insert var.index ptr = None)
        | _ -> ())
      decision;
    let gen_bindings ?(first = false) body =
      body.bindings#iter (fun did var ->
          let ptr = vars#unsafe_get var.index in
          let v =
            match ptrs#get var.index with
            | _ when first -> v
            | Some v -> v
            | None -> v
          in
          lcx#bx#store v ptr span;
          assert (lcx#locals#insert did.inner ptr = None))
    in
    let lower_body prev body =
      match bbs#get body.index with
      | Some (_, value) when not body.is_or ->
          gen_bindings body;
          value
      | Some (_, value) ->
          let prev = Option.get prev in
          body.bindings#iter (fun did var ->
              let value = lcx#locals#unsafe_get did.inner in
              match value with
              | VReg { kind = Phi (_, args); _ } ->
                  args#push (Label prev, vars#unsafe_get var.index)
              | _ -> assert false);
          value
      | None when not body.is_or ->
          gen_bindings body;
          let value = lower (arms#get body.index) in
          assert (bbs#insert body.index (lcx#bx#block, value) = None);
          value
      | None ->
          let prev = Option.get prev in
          body.bindings#iter (fun did var ->
              let args = new vec in
              args#push (Label prev, vars#unsafe_get var.index);
              let value = lcx#bx#phi (tcx#ptr Mut var.ty) args span in
              ignore @@ lcx#locals#insert did.inner value);
          let value = lower (arms#get body.index) in
          assert (bbs#insert body.index (lcx#bx#block, value) = None);
          value
    in
    let check_if_present = function
      | Success body -> Option.map fst (bbs#get body.index)
      | _ -> None
    in
    let rec go ?prev ?(first = false) = function
      | Success body when lett && first ->
          gen_bindings ~first body;
          lcx#bx#nop
      | Success body -> lower_body prev body
      | Switch (var, cases, default) ->
          let v = if first then v else ptrs#unsafe_get var.index in
          let disc =
            if tcx#adt_kind var.ty |> Option.is_some
            then lcx#bx#discriminant v span
            else v
          in
          let switch_bb = lcx#bx#block in
          let join = Basicblock.create () in
          let args = new vec in
          let phi_args = new vec in
          cases#iter (fun case ->
              let bb = Basicblock.create () in
              lcx#append_block_with_builder bb;
              let const =
                match case.cons with
                | Cons (ty, idx) ->
                    let payload = lcx#bx#payload v idx span in
                    let ty' = tcx#tuple_of_variant ty idx in
                    case.args#iteri (fun i var ->
                        let src = lcx#bx#gep ty' var.ty payload i span in
                        let value = lcx#bx#move src span in
                        ignore @@ ptrs#insert var.index value;
                        let ptr = vars#unsafe_get var.index in
                        lcx#bx#store value ptr span);
                    lcx#bx#const_int tcx#types.u8 idx
                | Int v -> lcx#bx#const_int var.ty v
                | True -> lcx#bx#const_bool var.ty true
                | False -> lcx#bx#const_bool var.ty false
                | Range _ -> assert false
              in
              match check_if_present case.body with
              | Some bb' ->
                  lcx#with_block bb (fun () -> lcx#bx#jmp (Label bb'));
                  let value = go ~prev:bb case.body in
                  args#push (Label bb, const);
                  phi_args#push (Label bb', value)
              | None ->
                  let bb' = Basicblock.create () in
                  lcx#with_block bb (fun () -> lcx#bx#jmp (Label bb'));
                  lcx#append_block_with_builder bb';
                  let value = go ~prev:bb case.body in
                  lcx#bx#jmp (Label join);
                  args#push (Label bb, const);
                  phi_args#push (Label lcx#bx#block, value));
          let default =
            Option.map
              (fun decision ->
                let prev = lcx#bx#block in
                let bb = Basicblock.create () in
                lcx#append_block_with_builder bb;
                let value = go ~prev decision in
                lcx#bx#jmp (Label join);
                phi_args#push (Label lcx#bx#block, value);
                Label bb)
              default
          in
          lcx#with_block switch_bb (fun () ->
              lcx#bx#switch ?default disc args);
          lcx#append_block_with_builder join;
          if ty = tcx#types.unit
          then lcx#bx#nop
          else lcx#bx#phi ty phi_args span
      | Failure -> assert false
    in
    go ~first:true decision
  and lower e =
    let v = lower' e in
    tcx#get_adjustment e.expr_id
    |> Option.map (fun adjust ->
           let open Adjustment in
           let ArrayToSlice = adjust.kind in
           lcx#bx#coercion ArrayToSlice v adjust.target e.expr_span)
    |> Option.value ~default:v
  and lower' e =
    let ty = expr_ty e in
    match e.expr_kind with
    | Binary (kind, left, right) ->
        let lazy_eval value =
          let open Ir in
          let left = lower left in
          let bb = lcx#bx#block in
          let right_bb = Basicblock.create () in
          let join_bb = Basicblock.create () in
          let join_label = Label join_bb in
          let right_label = Label right_bb in
          if value
          then lcx#bx#br left join_label right_label
          else lcx#bx#br left right_label join_label;
          lcx#append_block_with_builder right_bb;
          let right = lower right in
          let right_bb = lcx#bx#block in
          lcx#bx#jmp join_label;
          lcx#append_block_with_builder join_bb;
          let branches = new vec in
          branches#push (Label bb, lcx#bx#const_bool tcx#types.bool value);
          branches#push (Label right_bb, right);
          let phi = lcx#bx#phi ty branches e.expr_span in
          phi
        in
        let inst_kind = binary_kind_to_inst kind in
        (match inst_kind, tcx#types.bool = ty with
         | And, true -> lazy_eval false
         | Or, true -> lazy_eval true
         | _ ->
             let left = lower left in
             let right = lower right in
             lcx#bx#binary inst_kind left right e.expr_span)
    | Lit lit -> lower_lit lit ty
    | Path path ->
        let res = tcx#res_map#unsafe_get path.path_id in
        (match res with
         | Local (_, id) ->
             let ptr = lcx#locals#unsafe_get id in
             lcx#bx#move ptr path.span
         | Def (id, (Fn | AssocFn)) ->
             let subst = Fn.subst ty in
             let instance = { def = Fn id; subst = Subst subst } in
             Global (Fn instance)
         | Def (id, Cons) -> lower_variant ty id
         | Def (id, Intrinsic) ->
             let subst = Fn.subst ty in
             let instance = { def = Intrinsic id; subst = Subst subst } in
             Global (Fn instance)
         | _ -> assert false)
    | Call (expr, args) ->
        let ty = expr_ty expr in
        let args' = map args (fun arg -> lower arg) in
        let fn = lower expr in
        (match fn with
         | VReg { kind = Aggregate (_, args); _ } ->
             args#append args';
             fn
         | _ -> lcx#bx#call ty fn args' e.expr_span)
    | Deref expr ->
        let ptr = lower expr in
        lcx#bx#move ptr e.expr_span
    | Ref (_, expr) -> lcx#bx#bitcast (lower_lvalue expr) ty e.expr_span
    | If { cond; then_block; else_block; _ } ->
        let open Ir in
        let cond = lower cond in
        let then_bb = Basicblock.create () in
        let last_then_bb = ref then_bb in
        let else_bb = Basicblock.create () in
        let last_else_bb = ref else_bb in
        let join_bb = Basicblock.create () in
        let then_label = Label then_bb in
        let else_label = Label else_bb in
        lcx#bx#br cond then_label else_label;
        lcx#append_block_with_builder then_bb;
        let true' = lower_block lcx then_block in
        last_then_bb := lcx#bx#block;
        let false' = ref None in
        lcx#bx#jmp (Label join_bb);
        lcx#append_block_with_builder else_bb;
        (match else_block with
         | Some else_block ->
             false' := Some (lower else_block);
             last_else_bb := lcx#bx#block
         | None -> ());
        lcx#bx#jmp (Label join_bb);
        lcx#append_block_with_builder join_bb;
        (match !ty with
         | Unit -> lcx#bx#nop
         | _ ->
             let branches = new vec in
             branches#push (Label !last_then_bb, true');
             branches#push (Label !last_else_bb, Option.get !false');
             lcx#bx#phi ty branches e.expr_span)
    | Block block -> lower_block lcx block
    | StructExpr { fields; _ } ->
        let f = map fields (fun (_, expr) -> lower expr) in
        Const { kind = Struct f; ty }
    | Field (expr, ident) ->
        let ptr = lower_field expr ident in
        lcx#bx#move ptr e.expr_span
    | Cast (expr, cty) ->
        let ty = expr_ty expr in
        let cty = tcx#ast_ty_to_ty cty in
        let value = lower expr in
        (match !ty, !cty with
         | _ when ty = cty -> value
         | Ref t0, Ptr t1 when t0 = t1 ->
             lcx#bx#bitcast value cty e.expr_span
         | (Fn _ | FnPtr _ | Ptr _), Ptr _ ->
             lcx#bx#bitcast value cty e.expr_span
         | Ptr _, Int _ -> lcx#bx#ptrtoint value cty e.expr_span
         | Int _, Ptr _ -> lcx#bx#inttoptr value cty e.expr_span
         | Int t0, Int t1 when tcx#sizeof_int_ty t0 < tcx#sizeof_int_ty t1 ->
             lcx#bx#zext value cty e.expr_span
         | Int _, Int _ -> lcx#bx#trunc value cty e.expr_span
         | _ ->
             print_endline
             @@ tcx#sess.parse_sess.sm#span_to_string e.expr_span.lo;
             assert false)
    | MethodCall (expr, seg, args) -> lower_method e expr seg args
    | Match (value, arms) ->
        let decision = tcx#get_decision_tree (local_def_id value.expr_id) in
        let v = lower value in
        let ty = expr_ty e in
        let exprs = map arms (fun arm -> arm.expr) in
        lower_match ty v decision exprs e.expr_span
    | Slice exprs ->
        let values = map exprs lower in
        lcx#bx#aggregate
          (if tcx#is_slice ty then Slice ty else Array ty)
          values
          e.expr_span
    | Index (expr, idx) ->
        let ty = expr_ty expr in
        let value = lower expr in
        let ispan = idx.expr_span in
        let idx = lower idx in
        let length = lcx#bx#length value e.expr_span in
        let cond = lcx#bx#binary GtEq idx length e.expr_span in
        let open Ir in
        let bt = Basicblock.create () in
        let bf = Basicblock.create () in
        lcx#bx#br cond (Label bt) (Label bf);
        lcx#append_block_with_builder bt;
        let loc = tcx#sess.parse_sess.sm#span_to_string ispan.lo in
        let msg = "  index out of bounds at " ^ loc ^ "\n" in
        lcx#bx#trap msg e.expr_span;
        lcx#bx#jmp (Label bf);
        lcx#append_block_with_builder bf;
        lcx#bx#index ty value idx e.expr_span
    | Hole -> lcx#bx#nop
  in
  lower_block' ()
;;
