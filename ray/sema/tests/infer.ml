open Sema
open Ast
open Token
open Infer
open Front
open Tokenizer
open Parser

let dummy_span = { start = ("", 0, 0, 0); ending = ("", 0, 0, 0) }

let dummy_env = Hashtbl.create 0

let parse_input input =
  let tokenizer = tokenize "<test>" input in
  let pctx = parse_ctx_create tokenizer input in
  let modd = parse_mod pctx in
  modd

let mk_expr expr_kind id =
  { expr_kind; expr_ty = None; expr_id = id; expr_span = dummy_span }

let infer_unify expr ty =
  let ctx = infer_ctx_create dummy_env in
  ignore (unify ctx (infer ctx expr) ty);
  Option.get expr.expr_ty = ty

let%test "infer literal int" =
  let ctx = infer_ctx_create dummy_env in
  let expr = mk_expr (Lit (LitInt 20)) 0 in
  infer ctx expr = Infer (IntVar { index = 0 })

let%test "infer literal bool" =
  let ctx = infer_ctx_create dummy_env in
  let expr = mk_expr (Lit (LitBool true)) 0 in
  infer ctx expr = Bool

let%test "infer literal bool" =
  let ctx = infer_ctx_create dummy_env in
  let expr = mk_expr (Lit (LitBool true)) 0 in
  infer ctx expr = Bool

let%test "infer + unify literal int" =
  let expr = mk_expr (Lit (LitInt 20)) 0 in
  infer_unify expr (Int I64)

let%test "unify let int binding" =
  let ctx = infer_ctx_create dummy_env in
  let expr = mk_expr (Lit (LitInt 20)) 0 in
  let expr2 = mk_expr (Path { segments = ["a"] }) 1 in
  let ty = infer ctx expr in
  assert (ty = Infer (IntVar { index = 0 }));
  Hashtbl.add ctx.ty_env.bindings "a" ty;
  let ty2 = infer ctx expr2 in
  ignore (unify ctx ty2 (Int I64));
  Hashtbl.find ctx.ty_env.bindings "a" = Int I64

let%test "infer" =
  let input =
    "\n    fn main() -> i32 {\n        let a = 20;\n        a\n    }\n    "
  in
  let modd = parse_input input in
  let ctx = infer_ctx_create dummy_env in
  let fn =
    match List.nth modd.items 0 with
    | Fn (fn, _) -> infer_func ctx fn; fn
    | _ -> assert false
  in
  let binding = List.nth (Option.get fn.body).block_stmts 0 in
  match binding with
  | Binding binding -> binding.binding_expr.expr_ty = Some (Int I32)
  | _ -> false
