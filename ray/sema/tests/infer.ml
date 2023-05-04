open Sema
open Ast
open Infer

let mk_expr expr_kind id = { expr_kind; expr_ty = None; expr_id = id }

let infer_unify expr ty =
  let ctx = infer_ctx_create () in
  unify ctx (infer ctx expr) ty;
  Option.get expr.expr_ty = ty

let%test "infer literal int" =
  let ctx = infer_ctx_create () in
  let expr = mk_expr (Lit (LitInt 20)) 0 in
  infer ctx expr = Int 0

let%test "infer literal bool" =
  let ctx = infer_ctx_create () in
  let expr = mk_expr (Lit (LitBool true)) 0 in
  infer ctx expr = Normal (Prim Bool)

let%test "infer literal bool" =
  let ctx = infer_ctx_create () in
  let expr = mk_expr (Lit (LitBool true)) 0 in
  infer ctx expr = Normal (Prim Bool)

let%test "infer + unify literal int" =
  let expr = mk_expr (Lit (LitInt 20)) 0 in
  infer_unify expr (Prim I64)

let%test "unify let int binding" =
  let ctx = infer_ctx_create () in
  let expr = mk_expr (Lit (LitInt 20)) 0 in
  let expr2 = mk_expr (Ident "a") 1 in
  let ty = infer ctx expr in
  assert (ty = Int 0);
  Hashtbl.add ctx.ty_env.bindings "a" ty;
  let ty2 = infer ctx expr2 in
  unify ctx ty2 (Prim I64);
  Hashtbl.find ctx.ty_env.bindings "a" = Normal (Prim I64)
