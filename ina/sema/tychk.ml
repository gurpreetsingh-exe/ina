open Ast
open Middle.Ty
open Middle.Ctx
open Middle.Def_id
open Errors
open Structures.Vec
open Structures.Hashmap
open Diagnostic
open Printf
open Utils.Panic
open Infer

(* TODO: it's becoming a common pattern to `equate` after using `check_expr`,
   so just `equate` variables in `check_expr` when a type is expected *)

let ( let* ) v f = Result.bind v f

type cx = {
    infcx: infer_ctx
  ; locals: (node_id, ty ref) hashmap
  ; mutable generics: typaram array
}

let create infcx = { infcx; locals = new hashmap; generics = [||] }

type 'a expected_found = {
    expected: 'a
  ; found: 'a
}

type ty_err =
  | MismatchTy of ty ref * ty ref
  | UninitializedFields of ident
  | UnknownField of ident * ident
  | NoFieldInPrimitiveType of ty ref
  | InvalidBinaryExpression of binary_kind * ty ref * ty ref
  | MethodNotFound of ty ref * string
  | IntMismatch of int_ty expected_found
  | FloatMismatch of float_ty expected_found
  | InvalidDeref of ty ref
  | InvalidCall of ty ref
  | InvalidCast of ty ref * ty ref
  | AssocFnAsMethod of ty ref * string

type 'a tyck_result = ('a, ty_err) result

type expectation =
  | NoExpectation
  | ExpectTy of ty ref

exception TypeError of ty_err

let mismatch_int_ty expected ty span =
  let msg =
    sprintf
      "expected `%s`, found `%s`"
      (display_int_ty expected)
      (display_int_ty ty)
  in
  mk_err msg span
;;

let mismatch_float_ty expected ty span =
  let msg =
    sprintf
      "expected `%s`, found `%s`"
      (display_float_ty expected)
      (display_float_ty ty)
  in
  mk_err msg span
;;

let mismatch_args expected found span =
  let msg =
    sprintf
      "expected %d argument%s, found %d"
      expected
      (if expected = 1 then "" else "s")
      found
  in
  Diagnostic.create "mismatch arguments" |> label (Label.primary msg span)
;;

let mismatch_generic_args expected found span =
  let msg =
    sprintf
      "expected %d generic argument%s, found %d"
      expected
      (if expected = 1 then "" else "s")
      found
  in
  Diagnostic.create "mismatch generic arguments"
  |> label (Label.primary msg span)
;;

let missing_generic_args expected span =
  let msg =
    sprintf
      "expected %d generic argument%s"
      expected
      (if expected = 1 then "" else "s")
  in
  Diagnostic.create "function is missing generic arguments"
  |> label (Label.primary msg span)
;;

let unused_value span = mk_err "expression result unused" span

let uninitialized_fields name span =
  let msg = sprintf "`%s` has uninitialized fields" name in
  Diagnostic.create "uninitialized fields" |> label (Label.primary msg span)
;;

let unknown_field strukt name span =
  let msg = sprintf "`%s` has no field `%s`" strukt name in
  Diagnostic.create
    msg
    ~labels:[Label.primary (sprintf "has no field `%s`" name) span]
;;

let type_annotation_required name params span =
  let typenames =
    List.map (fun param -> sprintf "`%s`" param.name) params
    |> String.concat ", "
  in
  Diagnostic.create
    "type annotation required"
    ~labels:
      [
        Label.primary
          (sprintf
             "cannot infer type of type parameter%s %s in %s"
             (if List.length params > 1 then "s" else "")
             typenames
             name)
          span
      ]
;;

let ty_err_emit (tcx : tcx) err span =
  (* trace (); *)
  match err with
  | MismatchTy (expected, ty) ->
      let msg =
        sprintf
          "expected `%s`, found `%s`"
          (tcx#render_ty expected)
          (tcx#render_ty ty)
      in
      if ty <> tcx#types.err && expected <> tcx#types.err
      then
        Diagnostic.create "mismatch types"
        |> label (Label.primary msg span)
        |> tcx#emit
  | UninitializedFields name -> tcx#emit (uninitialized_fields name span)
  | UnknownField (strukt, name) -> tcx#emit (unknown_field strukt name span)
  | NoFieldInPrimitiveType ty ->
      let msg =
        sprintf "primitive type `%s` has no fields" (tcx#render_ty ty)
      in
      if ty <> tcx#types.err then tcx#emit (mk_err msg span)
  | InvalidBinaryExpression (kind, left, right) ->
      let msg =
        sprintf
          "cannot %s `%s` and `%s`"
          (match kind with
           | Add -> "add"
           | Sub -> "subtract"
           | Mul -> "multiply"
           | Div -> "divide"
           | Eq | NotEq | Gt | GtEq | Lt | LtEq -> "compare"
           | BitAnd | And -> "and"
           | BitOr | Or -> "or")
          (tcx#render_ty left)
          (tcx#render_ty right)
      in
      if left <> tcx#types.err && right <> tcx#types.err
      then
        Diagnostic.create "mismatch types" ~labels:[Label.primary msg span]
        |> tcx#emit
  | MethodNotFound (ty, name) ->
      let msg =
        sprintf "type `%s` has no method `%s`" (tcx#render_ty ty) name
      in
      if ty <> tcx#types.err then tcx#emit (mk_err msg span)
  | IntMismatch { expected; found } ->
      tcx#emit (mismatch_int_ty expected found span)
  | FloatMismatch { expected; found } ->
      tcx#emit (mismatch_float_ty expected found span)
  | InvalidDeref ty ->
      let msg = sprintf "`%s` cannot be dereferenced" (tcx#render_ty ty) in
      Diagnostic.create "invalid dereference"
      |> label (Label.primary msg span)
      |> tcx#emit
  | InvalidCall ty ->
      let msg = sprintf "`%s` is not callable" (tcx#render_ty ty) in
      Diagnostic.create "invalid function call"
      |> label (Label.primary msg span)
      |> tcx#emit
  | InvalidCast (of', to') ->
      let msg =
        sprintf
          "invalid cast of `%s` to `%s`"
          (tcx#render_ty of')
          (tcx#render_ty to')
      in
      tcx#emit (mk_err msg span)
  | AssocFnAsMethod (ty, name) ->
      let msg =
        sprintf "`%s::%s` is an associated function" (tcx#render_ty ty) name
      in
      Diagnostic.create "invalid function call"
      |> label (Label.primary msg span)
      |> tcx#emit
;;

let tychk_fn cx fn =
  let tcx = cx.infcx.tcx in
  (* let typeparams = new hashmap in *)
  let did = local_def_id fn.func_id in
  cx.generics <-
    (Generics.to_subst (tcx#generics_of did) tcx)#inner
    |> Array.map (function Middle.Ty.Ty ty ->
           !ty |> ( function Param p -> p | _ -> assert false ));
  let define id ty =
    tcx#create_def (local_def_id id) ty;
    ignore (cx.locals#insert id ty)
  in
  let resolve_expected = function
    | NoExpectation -> None
    | ExpectTy ty -> Some (Infer.resolve_vars cx.infcx ty)
  in
  let write_ty id ty =
    dbg "write_ty(id = %d, ty = %s)\n" id (tcx#render_ty ty);
    tcx#create_def (local_def_id id) ty
  in
  let int_unification_error (v : IntVid.e) =
    let expected, found = v in
    IntMismatch { expected; found }
  in
  let float_unification_error (v : FloatVid.e) =
    let expected, found = v in
    FloatMismatch { expected; found }
  in
  let ty_unification_error (v : TyVid.e) =
    let expected, found = v in
    MismatchTy (expected, found)
  in
  let unify_int_var vid value =
    let* _ =
      Result.map_error
        int_unification_error
        (IntUt.unify_var_value cx.infcx.int_ut vid (Some value))
    in
    Ok (tcx#int_ty_to_ty value)
  in
  let unify_float_var vid value =
    let* _ =
      Result.map_error
        float_unification_error
        (FloatUt.unify_var_value cx.infcx.float_ut vid (Some value))
    in
    Ok (tcx#float_ty_to_ty value)
  in
  let unify_ty_var vid value =
    let* _ =
      Result.map_error
        ty_unification_error
        (TyUt.unify_var_value cx.infcx.ty_ut vid (Some value))
    in
    Ok value
  in
  let rec equate (t0 : ty ref) (t1 : ty ref) : ty ref tyck_result =
    if t0 = t1
    then Ok t0
    else
      match !t0, !t1 with
      | Infer (IntVar i), Int t | Int t, Infer (IntVar i) ->
          unify_int_var i t
      | Infer (IntVar i0), Infer (IntVar i1) ->
          let* _ =
            Result.map_error
              int_unification_error
              (IntUt.unify_var_var cx.infcx.int_ut i0 i1)
          in
          Ok t0
      | Infer (FloatVar i), Float t | Float t, Infer (FloatVar i) ->
          unify_float_var i t
      | Infer (FloatVar i0), Infer (FloatVar i1) ->
          let* _ =
            Result.map_error
              float_unification_error
              (FloatUt.unify_var_var cx.infcx.float_ut i0 i1)
          in
          Ok t0
      | Infer (TyVar t0'), Infer (TyVar t1') ->
          let* _ =
            Result.map_error
              ty_unification_error
              (TyUt.unify_var_var cx.infcx.ty_ut t0' t1')
          in
          Ok t0
      | Infer (TyVar t), _ ->
          (match TyUt.probe_value cx.infcx.ty_ut t with
           | Some ty -> equate t1 ty
           | None -> unify_ty_var t t1)
      | _, Infer (TyVar t) ->
          (match TyUt.probe_value cx.infcx.ty_ut t with
           | Some ty -> equate t0 ty
           | None -> unify_ty_var t t0)
      | Infer _, _ | _, Infer _ -> Error (MismatchTy (t0, t1))
      | Ref (Mut, t0), Ref (Mut, t1) ->
          let* ty = equate t0 t1 in
          Ok (tcx#ref Mut ty)
      | Ref (Imm, t0), Ref (_, t1) ->
          let* ty = equate t0 t1 in
          Ok (tcx#ref Imm ty)
      | FnPtr t0', Fn (def_id, _) ->
          let t1' = tcx#get_fn def_id in
          if fnhash t0' = fnhash t1'
          then Ok t0
          else Error (MismatchTy (t0, t1))
      | Fn (did0, Subst s0), Fn (did1, Subst s1) ->
          unify_adt_or_fn did0 did1 s0 s1 t0 t1 tcx#fn
      | Adt (did0, Subst s0), Adt (did1, Subst s1) ->
          unify_adt_or_fn did0 did1 s0 s1 t0 t1 tcx#adt
      | _ -> Error (MismatchTy (t0, t1))
  and unify_adt_or_fn did0 did1 s0 s1 t0 t1 f =
    if did0 = did1 && s0#len = s1#len
    then (
      let res = map2 s0 s1 (fun (Ty t0) (Ty t1) -> equate t0 t1) in
      let err = find (function Error e -> Some e | _ -> None) res in
      match err with
      | Some err -> Error err
      | None ->
          let subst : generic_arg vec = new vec in
          for i = 0 to s0#len - 1 do
            let (Ty t0) = s0#get i in
            let (Ty t1) = s1#get i in
            match equate t0 t1 with
            | Ok ty -> subst#push (Ty ty)
            | Error _ -> assert false
          done;
          Ok (f did0 (Subst subst)))
    else Error (MismatchTy (t0, t1))
  in
  let fold_int_ty intvid old_ty =
    let opt_ty = IntUt.probe_value cx.infcx.int_ut intvid in
    let ty =
      match opt_ty with
      | Some ty -> tcx#int_ty_to_ty ty
      | None -> tcx#types.i32
    in
    tcx#invalidate !old_ty ty;
    ty
  in
  let fold_float_ty floatvid old_ty =
    let opt_ty = FloatUt.probe_value cx.infcx.float_ut floatvid in
    let ty =
      match opt_ty with
      | Some ty -> tcx#float_ty_to_ty ty
      | None -> tcx#types.f32
    in
    tcx#invalidate !old_ty ty;
    ty
  in
  let rec fold_ty ty =
    match !ty with
    | Infer (IntVar i) -> fold_int_ty i ty
    | Infer (FloatVar f) -> fold_float_ty f ty
    | Ptr (m, ty) -> tcx#ptr m (fold_ty ty)
    | Ref (m, ty) -> tcx#ref m (fold_ty ty)
    | _ -> ty
  in
  let last_stmt_span block =
    Option.map (fun e -> e.expr_span) block.last_expr
  in
  let rec check_block block =
    block.block_stmts#iter check_stmt;
    match block.last_expr with
    | Some expr -> check_expr expr NoExpectation
    | None -> tcx#types.unit
  and check_block_with_expected block expected =
    block.block_stmts#iter check_stmt;
    match block.last_expr with
    | Some expr ->
        let ty = check_expr expr expected in
        (match expected with
         | ExpectTy expected ->
             (match equate expected ty with
              | Ok _ -> ()
              | Error e -> ty_err_emit tcx e expr.expr_span);
             expected
         | NoExpectation -> ty)
    | None ->
        (match expected with
         | ExpectTy expected ->
             ty_err_emit
               tcx
               (MismatchTy (expected, tcx#types.unit))
               block.block_span;
             expected
         | NoExpectation -> tcx#types.unit)
  and check_stmt stmt =
    match stmt with
    | Assign (expr1, expr2) ->
        let left = check_expr expr1 NoExpectation in
        let right = check_expr expr2 NoExpectation in
        (match equate left right with
         | Ok _ -> ()
         | Error e -> ty_err_emit tcx e expr2.expr_span)
    | Stmt expr | Expr expr ->
        let _ = check_expr expr NoExpectation in
        ()
        (* if ty <> Unit && ty <> Err *)
        (* then tcx#emit (unused_value expr.expr_span) *)
    | Binding { binding_pat; binding_ty; binding_expr; binding_id; _ } ->
        (match binding_pat with
         | PatIdent _ ->
             (match binding_ty with
              | Some expected ->
                  let expected = tcx#ast_ty_to_ty expected in
                  let ty = check_expr binding_expr (ExpectTy expected) in
                  define binding_id expected;
                  (match equate expected ty with
                   | Ok _ -> ()
                   | Error e -> ty_err_emit tcx e binding_expr.expr_span)
              | None ->
                  let ty = check_expr binding_expr NoExpectation in
                  define binding_id ty))
    | Assert (cond, _) -> ignore (check_expr cond (ExpectTy tcx#types.bool))
  and check_expr expr expected =
    let ty = check_expr_kind expr expected in
    let ty = resolve_vars cx.infcx ty in
    write_ty expr.expr_id ty;
    ty
  and check_generic_args ty args span f =
    match !ty, args with
    | (Middle.Ty.Adt (did, _) | Fn (did, _)), None
      when (tcx#generics_of did).params#empty ->
        ty
    | (Adt (did, _) | Fn (did, _)), None ->
        tcx#emit
        @@ missing_generic_args (tcx#generics_of did).params#len span;
        tcx#types.err
    | (Adt (did, _) | Fn (did, _)), Some args ->
        let subst =
          let generics = tcx#generics_of did in
          if args#len <> generics.params#len
          then
            tcx#emit
            @@ mismatch_generic_args generics.params#len args#len span;
          let subst =
            map args (fun arg : generic_arg -> Ty (tcx#ast_ty_to_ty arg))
          in
          let psubst' = Generics.to_subst_parent generics tcx in
          psubst'#append subst;
          psubst'
        in
        f did (Subst subst)
    | FnPtr _, Some _ ->
        (* TODO(error): function pointers cannot be generic *)
        assert false
    | Err, _ -> ty
    | _ -> assert false
  and infer_type_argument_from_expr expr typaram subst =
    let expected = SubstFolder.fold_ty tcx typaram subst in
    match tcx#get_ty_params expected with
    | [] ->
        let ty = check_expr expr (ExpectTy expected) in
        (match equate expected ty with
         | Ok _ -> ()
         | Error e -> ty_err_emit tcx e expr.expr_span)
    | _ ->
        let ty = check_expr expr NoExpectation in
        (match tcx#unfold_ty_param expected ty with
         | Ok pairs ->
             List.iter
               (fun ({ index; _ }, ty) ->
                 subst#set index (Ty ty : generic_arg))
               pairs
         | Error _ ->
             ty_err_emit tcx (MismatchTy (expected, ty)) expr.expr_span)
  and infer_generic_args ty f =
    let args = tcx#get_subst ty |> Option.get in
    let args =
      map args (fun (Ty ty) : generic_arg ->
          if tcx#is_generic ty then Ty (infcx_new_ty_var cx.infcx) else Ty ty)
    in
    match !ty with Adt (did, _) -> f did (Subst args) | _ -> assert false
  and mk_type_constructor ty id args =
    if args#empty
    then ty
    else
      match tcx#get_subst ty with
      | Some subst ->
          let subst = Subst subst in
          tcx#fn_with_sig ~subst id args ty false Default
      | None -> tcx#fn_ptr args ty false Default
  and check_path path =
    tcx#res_map#unsafe_get path.path_id |> function
    | Def (id, (Adt | Struct)) ->
        let ty = tcx#get_def id in
        let last = Option.get path.segments#last in
        (match last.args with
         | Some args -> check_generic_args ty (Some args) path.span tcx#adt
         | None -> ty)
    | Def (id, Cons) ->
        let (Variant variant) = tcx#get_variant id in
        let fargs = map variant.fields (function Field { ty; _ } -> ty) in
        let key = tcx#def_key id in
        let parenid = Option.get key.parent in
        let adtty = tcx#get_def parenid in
        (* TODOOOOOOOO: write the constructor in tcx.constructor_def *)
        (match path.segments#len with
         | 0 -> assert false
         | 1 ->
             let last = path.segments#get 0 in
             let adtty =
               match last.args with
               | Some args ->
                   check_generic_args adtty (Some args) path.span tcx#adt
               | None -> infer_generic_args adtty tcx#adt
             in
             mk_type_constructor adtty id fargs
         | _ -> mk_type_constructor adtty id fargs)
    | Def (id, (Fn | Intrinsic)) ->
        let ty = tcx#get_def id in
        let last = Option.get path.segments#last in
        (match last.args with
         | Some args -> check_generic_args ty (Some args) path.span tcx#fn
         | None -> ty)
    | Def (id, AssocFn) ->
        let ty = tcx#get_def id in
        let last = Option.get path.segments#last in
        let second_last = path.segments#get (-2) in
        let adtty =
          tcx#res_map#unsafe_get second_last.id |> function
          | Def (id, Struct) ->
              let ty = tcx#get_def id in
              let ty =
                match second_last.args with
                | Some args ->
                    check_generic_args ty (Some args) path.span tcx#adt
                | None -> ty
              in
              ty
          | _ -> assert false
        in
        let ty =
          match last.args with
          | Some args -> check_generic_args ty (Some args) path.span tcx#fn
          | None -> ty
        in
        (match tcx#get_subst adtty with
         | Some subst -> SubstFolder.fold_ty tcx ty subst
         | None -> ty)
    | Local (_, id) -> cx.locals#unsafe_get id
    | Err | Def (_, TyParam) -> tcx#types.err
    | _ ->
        print_endline @@ tcx#sess.parse_sess.sm#span_to_string path.span.lo;
        assert false
  and check_arguments ?(is_variadic = false) pexpr exprs args =
    if (not is_variadic) && exprs#len <> args#len
    then tcx#emit @@ mismatch_args args#len exprs#len pexpr.expr_span
  and check_call pexpr exprs { args; ret; is_variadic; _ } =
    check_arguments pexpr exprs args ?is_variadic:(Some is_variadic);
    exprs#iteri (fun i arg ->
        let expected =
          if i < args#len then ExpectTy (args#get i) else NoExpectation
        in
        let ty = check_expr arg expected in
        if i < args#len
        then
          match equate (args#get i) ty with
          | Ok _ -> ()
          | Error e -> ty_err_emit tcx e arg.expr_span);
    ret
  and check_method ty name pexpr exprs fnsig =
    let { args; ret; _ } = fnsig in
    if args#empty
    then (
      ty_err_emit tcx (AssocFnAsMethod (ty, name)) pexpr.expr_span;
      ret)
    else
      let args' = new vec in
      args'#copy args;
      let first = tcx#autoderef (args'#get 0) in
      args'#pop_front;
      let args = args' in
      if first <> ty
      then ty_err_emit tcx (AssocFnAsMethod (ty, name)) pexpr.expr_span;
      if exprs#len <> args#len
      then tcx#emit @@ mismatch_args args#len exprs#len pexpr.expr_span;
      exprs#iteri (fun i arg ->
          let expected =
            if i < args#len then ExpectTy (args#get i) else NoExpectation
          in
          let ty = check_expr arg expected in
          if i < args#len
          then
            match equate (args#get i) ty with
            | Ok _ -> ()
            | Error e -> ty_err_emit tcx e arg.expr_span);
      ret
  and check_expr_kind expr expected =
    match expr.expr_kind with
    | Binary (kind, left, right) ->
        let expected =
          match kind with
          | And | Or -> ExpectTy tcx#types.bool
          | _ -> NoExpectation
        in
        let left = check_expr left expected in
        let right = check_expr right expected in
        let ty =
          match equate left right with
          | Ok ty -> ty
          | Error _ ->
              let e = InvalidBinaryExpression (kind, left, right) in
              ty_err_emit tcx e expr.expr_span;
              left
        in
        (match kind with
         | Lt | LtEq | Gt | GtEq | Eq | NotEq -> tcx#types.bool
         | _ -> ty)
    | Call (expr, args) ->
        let ty = check_expr expr NoExpectation in
        (match !ty with
         | FnPtr fnsig -> check_call expr args fnsig
         | Fn (def_id, Subst subst) when Fn.is_generic ty ->
             let fnsig = tcx#get_fn def_id in
             let subst0 = new vec in
             subst0#copy subst;
             let subst = subst0 in
             check_arguments expr args fnsig.args;
             let maybe_infer_typaram i arg =
               if i < fnsig.args#len
               then
                 infer_type_argument_from_expr arg (fnsig.args#get i) subst
             in
             args#iteri maybe_infer_typaram;
             let fn = tcx#fn def_id (Subst subst) in
             let params =
               tcx#get_ty_params fn
               |> List.filter (fun param ->
                      not @@ Array.mem param cx.generics)
             in
             if params <> []
             then
               type_annotation_required
                 (tcx#describe_def_id def_id)
                 params
                 expr.expr_span
               |> tcx#emit;
             write_ty expr.expr_id fn;
             SubstFolder.fold_ty tcx fnsig.ret subst
         | Fn (def_id, _) -> tcx#get_fn def_id |> check_call expr args
         | Err ->
             args#iter (fun arg -> ignore (check_expr arg NoExpectation));
             ty
         | _ ->
             ty_err_emit tcx (InvalidCall ty) expr.expr_span;
             tcx#types.err)
    | Path path -> check_path path
    | Lit lit ->
        let go new_var = function
          | Some { contents = Infer (TyVar vid) } ->
              let ty = new_var cx.infcx in
              (match unify_ty_var vid ty with
               | Ok _ -> ()
               | Error e -> ty_err_emit tcx e expr.expr_span);
              ty
          | Some ty ->
              let found = new_var cx.infcx in
              ty_err_emit tcx (MismatchTy (ty, found)) expr.expr_span;
              ty
          | None -> new_var cx.infcx
        in
        (match lit with
         | LitInt _ ->
             (match resolve_expected expected with
              | Some ({ contents = Int _ } as ty) -> ty
              | opt -> go infcx_new_int_var opt)
         | LitFloat _ ->
             (match resolve_expected expected with
              | Some ({ contents = Float _ } as ty) -> ty
              | opt -> go infcx_new_float_var opt)
         | LitStr _ -> tcx#types.str
         | LitBool _ -> tcx#types.bool)
    | Block block -> check_block_with_expected block expected
    | Deref expr ->
        let ty = check_expr expr NoExpectation in
        (match !ty with
         | Ptr (_, ty) | Ref (_, ty) -> ty
         | _ ->
             ty_err_emit tcx (InvalidDeref ty) expr.expr_span;
             tcx#types.err)
    | Ref (m, expr') ->
        let m = tcx#ast_mut_to_mut m in
        (match resolve_expected expected with
         | Some expected ->
             let ty = check_expr expr' NoExpectation in
             (match equate expected (tcx#ref m ty) with
              | Ok _ -> ()
              | Error e -> ty_err_emit tcx e expr.expr_span);
             expected
         | None ->
             let ty = check_expr expr' NoExpectation in
             tcx#ref m ty)
    | If { cond; then_block; else_block; _ } ->
        ignore (check_expr cond (ExpectTy tcx#types.bool));
        let if_ty = check_block_with_expected then_block expected in
        Option.fold
          ~some:(fun expr' ->
            let else_ty = check_expr expr' expected in
            Result.fold
              ~ok:(fun ty -> ty)
              ~error:(fun _ ->
                let labels =
                  [
                    Label.secondary
                      "if and else have incompatible types"
                      expr.expr_span
                  ]
                  @ (last_stmt_span then_block
                     |> Option.map (fun span ->
                            [Label.secondary "expected because of this" span])
                     |> Option.value ~default:[])
                  @ ((match expr'.expr_kind with
                      | Block block -> block
                      | _ -> assert false)
                     |> last_stmt_span
                     |> Option.map (fun span ->
                            [
                              Label.primary
                                (sprintf
                                   "expected `%s`, found `%s`"
                                   (tcx#render_ty if_ty)
                                   (tcx#render_ty else_ty))
                                span
                            ])
                     |> Option.value ~default:[])
                in
                Diagnostic.create "mismatch types" ~labels |> tcx#emit;
                else_ty)
              (equate if_ty else_ty))
          ~none:tcx#types.unit
          else_block
    | StructExpr expr ->
        let name = expr.struct_name.segments#join "::" (fun s -> s.ident) in
        let ty = check_path expr.struct_name in
        let (Variant variant) = tcx#non_enum_variant ty |> Option.get in
        let remaining_fields = new hashmap in
        variant.fields#iter (fun (Field { ty; name }) ->
            remaining_fields#insert' name ty);
        let ty =
          match !ty with
          | Adt (def_id, Subst subst) when tcx#is_generic ty ->
              let subst' = new vec in
              subst'#copy subst;
              let maybe_infer_field (ident, expr) =
                match remaining_fields#remove ident with
                | Some expected ->
                    infer_type_argument_from_expr expr expected subst'
                | None ->
                    let err = UnknownField (name, ident) in
                    ty_err_emit tcx err expr.expr_span
              in
              expr.fields#iter maybe_infer_field;
              let adt = tcx#adt def_id (Subst subst') in
              let params =
                tcx#get_ty_params adt
                |> List.filter (fun param ->
                       not @@ Array.mem param cx.generics)
              in
              if params <> []
              then
                type_annotation_required
                  (tcx#describe_def_id def_id)
                  params
                  expr.struct_expr_span
                |> tcx#emit;
              adt
          | _ ->
              let check_field (ident, expr) =
                match remaining_fields#remove ident with
                | Some expected ->
                    let ty = check_expr expr (ExpectTy expected) in
                    (match equate expected ty with
                     | Ok _ -> ()
                     | Error e -> ty_err_emit tcx e expr.expr_span)
                | None ->
                    let err = UnknownField (name, ident) in
                    ty_err_emit tcx err expr.expr_span
              in
              expr.fields#iter check_field;
              ty
        in
        (if remaining_fields#len <> 0
         then
           (* TODO: display all missing fields *)
           let err = UninitializedFields name in
           ty_err_emit tcx err expr.struct_expr_span);
        ty
    | Field (expr, ident) ->
        let ty = check_expr expr NoExpectation in
        let ty = tcx#autoderef ty in
        (match !ty with
         | Err -> ty
         | _ ->
             (match tcx#non_enum_variant ty with
              | Some (Variant variant) ->
                  find
                    (fun (Field { name; ty }) ->
                      if name = ident then Some ty else None)
                    variant.fields
                  |> ( function
                  | Some ty -> ty
                  | None ->
                      let name =
                        (tcx#def_key variant.def_id).data |> function
                        | TypeNs name -> name
                        | _ -> assert false
                      in
                      let err = UnknownField (name, ident) in
                      ty_err_emit tcx err expr.expr_span;
                      tcx#types.err )
              | None ->
                  Diagnostic.create
                    "invalid field access on primitive type"
                    ~labels:
                      [
                        Label.primary
                          (sprintf
                             "`%s` has no field `%s`"
                             (tcx#render_ty ty)
                             ident)
                          expr.expr_span
                      ]
                  |> tcx#emit;
                  tcx#types.err))
    | Cast (expr, ty) ->
        let cty = tcx#ast_ty_to_ty ty in
        let ty = check_expr expr NoExpectation in
        let ty = fold_ty ty in
        (match !ty, !cty with
         | Ref t0, Ptr t1 when t0 = t1 -> cty
         | (Fn _ | FnPtr _ | Ptr _), Ptr _ -> cty
         | (Ptr _ | Int _), (Int _ | Ptr _) -> cty
         | _ ->
             ty_err_emit tcx (InvalidCast (ty, cty)) expr.expr_span;
             cty)
    | MethodCall (expr', seg, args) ->
        let name = seg.ident in
        let ty = check_expr expr' NoExpectation in
        let ty = tcx#autoderef ty in
        (match tcx#lookup_method ty name with
         | { contents = Err } as m ->
             Diagnostic.create
               (sprintf "method `%s` not found" name)
               ~labels:
                 [
                   Label.primary
                     (sprintf
                        "`%s` has no method `%s`"
                        (tcx#render_ty ty)
                        name)
                     expr.expr_span
                 ]
             |> tcx#emit;
             m
         | method' ->
             let method' =
               Option.fold
                 ~none:method'
                 ~some:(fun subst -> SubstFolder.fold_ty tcx method' subst)
                 (tcx#get_subst ty)
             in
             let method' =
               check_generic_args method' seg.args seg.span tcx#fn
             in
             (match !method' with
              | Fn (did, subst) ->
                  let fnsig = tcx#get_fn did in
                  tcx#subst fnsig subst |> check_method ty name expr args
              | FnPtr fnsig -> check_method ty name expr args fnsig
              | _ ->
                  ty_err_emit tcx (InvalidCall ty) expr.expr_span;
                  tcx#types.err))
  in
  let ty = tcx#get_def did in
  let ret = Fn.ret tcx ty in
  (* TODO: display better error span *)
  let span =
    match fn.fn_sig.ret_ty with
    | Some ty -> ty.span
    | None -> fn.fn_sig.fn_span
  in
  (match fn.body with
   | Some block ->
       fn.fn_sig.args#iter (fun { ty; arg_id; _ } ->
           define arg_id (tcx#ast_ty_to_ty ty));
       let ty = check_block block in
       (match equate ret ty with
        | Ok _ -> ()
        | Error e -> ty_err_emit tcx e span)
   | None -> ());
  tcx#iter_infer_vars (function
      | v, IntVar i -> ignore @@ fold_int_ty i v
      | v, FloatVar f -> ignore @@ fold_float_ty f v
      | v, TyVar t ->
          let opt_ty = TyUt.probe_value cx.infcx.ty_ut t in
          tcx#invalidate
            !v
            (match opt_ty with Some ty -> fold_ty ty | None -> assert false))
;;

let rec tychk cx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> tychk_fn cx func
    | Foreign (funcs, _) -> funcs#iter (fun f -> tychk_fn cx f)
    | Impl { items; _ } -> items#iter (function AssocFn f -> tychk_fn cx f)
    | Type _ | ExternMod _ | Using _ -> ()
    | Mod m ->
        (match m.resolved_mod with Some modd -> tychk cx modd | None -> ())
  in
  modd.items#iter f
;;
