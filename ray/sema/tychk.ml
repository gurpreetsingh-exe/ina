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

type cx = {
    infcx: infer_ctx
  ; locals: (node_id, ty) hashmap
}

let create infcx = { infcx; locals = new hashmap }

type ty_err =
  | MismatchTy of ty * ty
  | UninitializedFields of ident
  | UnknownField of ident * ident
  | NoFieldInPrimitiveType of ty
  | InvalidBinaryExpression of binary_kind * ty * ty
  | MethodNotFound of ty * string

type expectation =
  | NoExpectation
  | ExpectTy of ty

exception TypeError of ty_err

let mismatch_ty expected ty span =
  let msg =
    sprintf "expected `%s`, found `%s`" (render_ty expected) (render_ty ty)
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
;;

let tychk_fn cx fn =
  let tcx = cx.infcx.tcx in
  let define id ty = ignore (cx.locals#insert id ty) in
  let resolve_expectation = function
    | NoExpectation -> None
    | ExpectTy ty -> Some (Infer.resolve_vars cx.infcx ty)
  in
  let write_ty id ty =
    dbg "write_ty(id = %d, ty = %s)\n" id (render_ty ty);
    ignore (tcx#node_id_to_ty#insert id ty)
  in
  let equate (t0 : ty) (t1 : ty) =
    match t0, t1 with
    | Infer (IntVar i), (Int _ as t) | (Int _ as t), Infer (IntVar i) ->
        IntUt.unify_var_value cx.infcx.int_ut i (Some t)
    | Infer (IntVar i0), Infer (IntVar i1) ->
        IntUt.unify_var_var cx.infcx.int_ut i0 i1
    | _ -> Ok ()
  in
  let rec check_block block =
    block.block_stmts#iter check_stmt;
    match block.last_expr with
    | Some expr -> check_expr expr NoExpectation
    | None -> Unit
  and check_block_with_expected block expected =
    block.block_stmts#iter check_stmt;
    match block.last_expr with
    | Some expr ->
        let ty = check_expr expr expected in
        (match expected with
         | ExpectTy expected ->
             ignore (equate ty expected);
             if ty != expected
             then tcx#emit (mismatch_ty expected ty expr.expr_span);
             expected
         | NoExpectation -> ty)
    | None -> Unit
  and check_stmt stmt =
    match stmt with
    | Assign (expr1, expr2) ->
        let left = check_expr expr1 NoExpectation in
        let right = check_expr expr2 NoExpectation in
        if left != right
        then ty_err_emit tcx (MismatchTy (left, right)) expr2.expr_span
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
                  if expected != ty
                  then
                    ty_err_emit
                      tcx
                      (MismatchTy (expected, ty))
                      binding_expr.expr_span
              | None ->
                  let ty = check_expr binding_expr NoExpectation in
                  define binding_id ty))
    | Assert _ -> ()
  and check_expr expr expectation =
    let ty = check_expr_kind expr expectation in
    let ty = resolve_vars cx.infcx ty in
    write_ty expr.expr_id ty;
    ty
  and check_expr_kind expr expectation =
    match expr.expr_kind with
    | Binary (kind, left, right) ->
        let left, right =
          check_expr left expectation, check_expr right expectation
        in
        if left != right
        then
          ty_err_emit
            tcx
            (InvalidBinaryExpression (kind, left, right))
            expr.expr_span;
        left
    | Call (expr, args) ->
        let ty = check_expr expr NoExpectation in
        ty
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
             resolve_expectation expectation
             |> Option.map (fun (ty : ty) ->
                    match ty with Int _ -> ty | _ -> ty)
             |> ( function
             | Some ty -> ty
             | None -> infcx_new_int_var cx.infcx )
         | LitFloat _ -> tcx#types.f32
         | LitStr _ -> tcx#types.str
         | LitBool _ -> tcx#types.bool)
    | Block block -> check_block_with_expected block expectation
    | If _ | Deref _ | Ref _ | StructExpr _ | Field _ | Cast _ | MethodCall _
      ->
        assert false
  in
  let ty = tcx#node_id_to_ty#unsafe_get fn.func_id in
  let ret =
    match ty with
    | FnPtr { ret; _ } -> ret
    | _ ->
        dbg "expected function, found `%s`\n" (render_ty ty);
        assert false
  in
  match fn.body with
  | Some block ->
      fn.fn_sig.args#iter (fun { ty; arg_id; _ } ->
          define arg_id (tcx#ast_ty_to_ty ty));
      ignore (check_block_with_expected block (ExpectTy ret))
  | None -> ()
;;

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
