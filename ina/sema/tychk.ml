open Ast
open Middle.Ty
open Middle.Ctx
open Errors
open Structures.Vec
open Structures.Hashmap
open Diagnostic
open Printf
open Session
open Utils.Panic
open Infer

let ( let* ) v f = Result.bind v f

type cx = {
    infcx: infer_ctx
  ; locals: (node_id, ty ref) hashmap
}

let create infcx = { infcx; locals = new hashmap }

type 'a expected_found = {
    expected: 'a
  ; found: 'a
}

type ty_err =
  | MismatchTy of ty * ty
  | UninitializedFields of ident
  | UnknownField of ident * ident
  | NoFieldInPrimitiveType of ty
  | InvalidBinaryExpression of binary_kind * ty * ty
  | MethodNotFound of ty * string
  | IntMismatch of int_ty expected_found
  | FloatMismatch of float_ty expected_found

type 'a tyck_result = ('a, ty_err) result

type expectation =
  | NoExpectation
  | ExpectTy of ty ref

exception TypeError of ty_err

let mismatch_ty expected ty span =
  let msg =
    sprintf "expected `%s`, found `%s`" (render_ty expected) (render_ty ty)
  in
  mk_err msg span
;;

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

let invalid_binary_expr kind left right span =
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
      (render_ty left)
      (render_ty right)
  in
  mk_err msg span
;;

let unused_value span = mk_err "expression result unused" span

let uninitialized_fields name span =
  let msg = sprintf "`%s` has uninitialized fields" name in
  mk_err msg span
;;

let unknown_field strukt name span =
  let msg = sprintf "`%s` has no field `%s`" strukt name in
  mk_err msg span
;;

let no_field_in_prim_ty ty span =
  let msg = sprintf "primitive type `%s` has no fields" (render_ty ty) in
  mk_err msg span
;;

let method_not_found ty name span =
  let msg = sprintf "type `%s` has no method `%s`" (render_ty ty) name in
  mk_err msg span
;;

let invalid_call ty span =
  let msg = sprintf "`%s` is not callable" (render_ty ty) in
  mk_err msg span
;;

let ty_err_emit (tcx : tcx) err span =
  match err with
  | MismatchTy (expected, ty) ->
      if ty <> Err && expected <> Err
      then tcx#emit (mismatch_ty expected ty span)
  | UninitializedFields name -> tcx#emit (uninitialized_fields name span)
  | UnknownField (strukt, name) -> tcx#emit (unknown_field strukt name span)
  | NoFieldInPrimitiveType ty ->
      if ty <> Err then tcx#emit (no_field_in_prim_ty ty span)
  | InvalidBinaryExpression (kind, left, right) ->
      if left <> Err && right <> Err
      then tcx#emit (invalid_binary_expr kind left right span)
  | MethodNotFound (ty, name) ->
      if ty <> Err then tcx#emit (method_not_found ty name span)
  | IntMismatch { expected; found } ->
      tcx#emit (mismatch_int_ty expected found span)
  | FloatMismatch { expected; found } ->
      tcx#emit (mismatch_float_ty expected found span)
;;

let tychk_fn cx fn =
  let tcx = cx.infcx.tcx in
  let define id ty =
    ignore (tcx#node_id_to_ty#insert id ty);
    ignore (cx.locals#insert id ty)
  in
  let resolve_expected = function
    | NoExpectation -> None
    | ExpectTy ty -> Some (Infer.resolve_vars cx.infcx ty)
  in
  let write_ty id ty =
    dbg "write_ty(id = %d, ty = %s)\n" id (render_ty2 !ty);
    ignore (tcx#node_id_to_ty#insert id ty)
  in
  let int_unification_error (v : IntVid.e) =
    let expected, found = v in
    IntMismatch { expected; found }
  in
  let float_unification_error (v : FloatVid.e) =
    let expected, found = v in
    FloatMismatch { expected; found }
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
  let equate (t0 : ty ref) (t1 : ty ref) : ty ref tyck_result =
    if t0 == t1
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
      | Infer _, _ | _, Infer _ -> Error (MismatchTy (!t0, !t1))
      | _ -> Error (MismatchTy (!t0, !t1))
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
             tcx#emit
             @@ mismatch_ty !expected !(tcx#types.unit) block.block_span;
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
    | Assert _ -> ()
  and check_expr expr expected =
    let ty = check_expr_kind expr expected in
    let ty = resolve_vars cx.infcx ty in
    write_ty expr.expr_id ty;
    ty
  and check_expr_kind expr expected =
    match expr.expr_kind with
    | Binary (kind, left, right) ->
        let left, right =
          check_expr left expected, check_expr right expected
        in
        if left != right
        then
          ty_err_emit
            tcx
            (InvalidBinaryExpression (kind, !left, !right))
            expr.expr_span;
        left
    | Call (expr, args) ->
        let ty = check_expr expr NoExpectation in
        (match !ty with
         | FnPtr { args = arg_tys; ret; _ } ->
             if args#len <> arg_tys#len
             then
               tcx#emit @@ mismatch_args arg_tys#len args#len expr.expr_span;
             args#iteri (fun i arg ->
                 ignore
                   (check_expr
                      arg
                      (if i < arg_tys#len
                       then ExpectTy (tcx#intern @@ arg_tys#get i)
                       else NoExpectation)));
             tcx#intern ret
         | _ ->
             tcx#emit @@ invalid_call !ty expr.expr_span;
             ref Middle.Ty.Err)
    | Path path ->
        let res = tcx#res_map#unsafe_get path.path_id in
        let ty =
          match res with
          | Def (id, _) -> tcx#node_id_to_ty#unsafe_get id.inner
          | Local id -> cx.locals#unsafe_get id
          | _ -> assert false
        in
        ty
    | Lit lit ->
        (match lit with
         | LitInt _ ->
             (match resolve_expected expected with
              | Some ({ contents = Int _ } as ty) -> ty
              | _ -> infcx_new_int_var cx.infcx)
         | LitFloat _ ->
             (match resolve_expected expected with
              | Some ({ contents = Float _ } as ty) -> ty
              | _ -> infcx_new_float_var cx.infcx)
         | LitStr _ -> tcx#types.str
         | LitBool _ -> tcx#types.bool)
    | Block block -> check_block_with_expected block expected
    | If _ | Deref _ | Ref _ | StructExpr _ | Field _ | Cast _ | MethodCall _
      ->
        assert false
  in
  let ty = tcx#node_id_to_ty#unsafe_get fn.func_id in
  let ret =
    match !ty with
    | FnPtr { ret; _ } -> tcx#intern ret
    | _ ->
        dbg "expected function, found `%s`\n" (render_ty !ty);
        assert false
  in
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
  Array.iter
    (fun (v : IntUt.VarValue.t) ->
      let old_ty = Infer (IntVar v.parent) in
      let ty = tcx#int_ty_to_ty @@ Option.value v.value ~default:I32 in
      tcx#invalidate old_ty !ty)
    cx.infcx.int_ut.values
;;

(* tcx#node_id_to_ty#iter (fun k v -> printf "%d -> %s\n" k (render_ty !v)) *)

let rec tychk cx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> tychk_fn cx func
    | Foreign funcs -> funcs#iter (fun f -> tychk_fn cx f)
    | Impl { impl_items; _ } ->
        impl_items#iter (function AssocFn f -> tychk_fn cx f)
    | Type _ | Unit _ -> ()
    | Mod m ->
        (match m.resolved_mod with Some modd -> tychk cx modd | None -> ())
  in
  modd.items#iter f
;;
