open Ast
open Infer
open Token
open Front.Fmt

let ty_unwrap (ty : ty option) = Option.value ty ~default:Unit

type env = {
  parent : env option;
  bindings : (string, ty) Hashtbl.t;
}

let env_create parent : env = { parent; bindings = Hashtbl.create 0 }

type ty_ctx = {
  ty_env : env;
  func_map : (ident, ty) Hashtbl.t;
}

type ty_err =
  | MismatchTy of ty * ty
  | NoReturn of func

exception TypeError of ty_err

let ty_err_emit ty_err span =
  match ty_err with
  | MismatchTy (expected, ty) ->
      Printf.printf "\x1b[31;1m%s\x1b[0m: expected `%s`, found `%s`\n"
        (display_span span) (render_ty expected) (render_ty ty)
  | NoReturn func ->
      Printf.printf
        "\x1b[31;1m%s\x1b[0m: function `%s` returns nothing, but `%s` \
         expected\n"
        (display_span span) func.fn_sig.name
        (render_ty (ty_unwrap func.fn_sig.ret_ty))

let ty_ctx_create (infer_ctx : infer_ctx) =
  { ty_env = env_create None; func_map = infer_ctx.func_map }

let tychk_func (ty_ctx : ty_ctx) (func : func) =
  let { fn_sig = { ret_ty; fn_span; _ }; body; _ } = func in
  let rec fexpr expr =
    match expr.expr_kind with
    | If { cond; then_block; else_block } -> (
        fexpr cond;
        fblock then_block;
        match else_block with
        | Some else_block -> fblock else_block
        | None -> ())
    | Block body -> fblock body
    | _ -> ()
  and fblock body =
    List.iter f body.block_stmts;
    match body.last_expr with Some expr -> fexpr expr | None -> ()
  and f stmt =
    match stmt with
    | Assign (expr1, expr2) ->
        expr2.expr_ty <- expr1.expr_ty;
        fexpr expr1;
        fexpr expr2
    | Stmt expr | Expr expr -> fexpr expr
    | Binding ({ binding_pat; binding_ty; binding_expr; _ } as binding) -> (
        fexpr binding_expr;
        match binding_pat with
        | PatIdent ident -> (
            (let ty =
               match binding_expr.expr_ty with
               | Some ty ->
                   Hashtbl.add ty_ctx.ty_env.bindings ident ty;
                   ty
               | None -> assert false
             in
             match binding_ty with
             | Some expected ->
                 if expected <> ty then
                   ty_err_emit
                     (MismatchTy (expected, ty))
                     binding_expr.expr_span
             | None -> binding.binding_ty <- Some ty);
            let check_overflow _value ty =
              match ty with
              | Prim prim ->
                  let _ = integer_ranges prim in
                  ()
              | _ -> ()
            in
            match binding_expr.expr_kind with
            | Lit lit -> (
              match lit with
              | LitInt value ->
                  check_overflow value (Option.get binding_expr.expr_ty)
              | _ -> ())
            | _ -> ()))
  in
  let ret_ty = Option.value ret_ty ~default:Unit in
  match body with
  | Some body -> (
      List.iter f body.block_stmts;
      match body.last_expr with
      | Some expr -> fexpr expr
      | None -> (
        match ret_ty with
        | Unit -> ()
        | _ -> ty_err_emit (NoReturn func) fn_span))
  | None -> ()

let tychk ty_ctx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> tychk_func ty_ctx func
    | Import _ -> ()
    | _ -> assert false
  in
  List.iter f modd.items
