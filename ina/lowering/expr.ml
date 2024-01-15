open Ast
open Context
open Structures.Vec
open Middle.Def_id

let rec lower_block (lcx : lcx) block =
  let tcx = lcx#tcx in
  let expr_ty expr = tcx#get_def (local_def_id expr.expr_id) in
  let rec lower_block' () =
    let f stmt =
      match stmt with
      | Binding binding ->
          let { binding_expr; binding_id; binding_span; _ } = binding in
          let ty = tcx#get_def (local_def_id binding_id) in
          let ptr = lcx#bx#alloca ty binding_span in
          assert (lcx#locals#insert binding_id ptr = None);
          let src = lower binding_expr in
          lcx#bx#store src ptr binding_span
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
    | Ptr ty | Ref ty -> lower expr, ty
    | _ -> lower_lvalue expr, ty
  and lower_field expr ident =
    let ptr, ty = lower_autoderef expr in
    lcx#bx#gep ty ptr ident expr.expr_span
  and lower_method e expr name args =
    let first, ty = lower_autoderef expr in
    let first, ty =
      let open Middle.Ty in
      let method' = tcx#lookup_method ty name in
      match Fn.args tcx method' with
      | args when args#empty -> assert false
      | args ->
          (match !(args#get 0) with
           | Ref ty -> first, ty
           | _ -> lcx#bx#move first expr.expr_span, ty)
    in
    let id = tcx#lookup_method_def_id ty name in
    let instance = Ir.Inst.{ def = Fn id; subst = Subst (new vec) } in
    let fn = Ir.Inst.Global (Fn instance) in
    let ty = tcx#lookup_method ty name in
    let args' = new vec in
    args'#push first;
    args'#append @@ map args (fun arg -> lower arg);
    lcx#bx#call ty fn args' e.expr_span
  and lower_lvalue e =
    let ty = expr_ty e in
    match e.expr_kind with
    | Lit lit ->
        let ptr = lcx#bx#alloca ty e.expr_span in
        lcx#bx#store (lower_lit lit ty) ptr e.expr_span;
        ptr
    | Path path ->
        let res = tcx#res_map#unsafe_get path.path_id in
        (match res with
         | Local id -> lcx#locals#unsafe_get id
         | Def (id, (Fn | Intrinsic)) ->
             let subst = Middle.Ty.Fn.subst ty in
             let instance = Ir.Inst.{ def = Fn id; subst = Subst subst } in
             Global (Fn instance)
         | _ -> assert false)
    | Deref expr -> lower expr
    | Field (expr, ident) -> lower_field expr ident
    | MethodCall (expr, name, args) -> lower_method e expr name args
    | _ ->
        print_endline @@ tcx#sess.parse_sess.sm#span_to_string e.expr_span.lo;
        assert false
  and lower e =
    let ty = expr_ty e in
    match e.expr_kind with
    | Binary (kind, left, right) ->
        let lazy_eval value =
          let open Ir in
          let open Inst in
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
        let inst_kind = Ir.Inst.binary_kind_to_inst kind in
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
         | Local id ->
             let ptr = lcx#locals#unsafe_get id in
             lcx#bx#move ptr path.span
         | Def (id, (Fn | Intrinsic)) ->
             let subst = Middle.Ty.Fn.subst ty in
             let instance = Ir.Inst.{ def = Fn id; subst = Subst subst } in
             Global (Fn instance)
         | _ -> assert false)
    | Call (expr, args) ->
        let ty = expr_ty expr in
        let fn = lower expr in
        let args = map args (fun arg -> lower arg) in
        lcx#bx#call ty fn args e.expr_span
    | Deref expr ->
        let ptr = lower expr in
        lcx#bx#move ptr e.expr_span
    | Ref expr -> lcx#bx#bitcast (lower_lvalue expr) ty e.expr_span
    | If { cond; then_block; else_block; _ } ->
        let open Ir in
        let open Inst in
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
    | MethodCall (expr, name, args) -> lower_method e expr name args
  in
  lower_block' ()
;;
