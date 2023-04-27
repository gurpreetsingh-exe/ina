open Ast
open Front.Fmt

type infer_kind =
  | Int of node_id
  | Float of node_id
  | Normal of ty

let display_infer_kind = function
  | Int _ -> "int"
  | Float _ -> "float"
  | Normal ty -> render_ty ty

let ty_is_int (ty : ty) : bool =
  match ty with
  | Prim ty -> ( match ty with I64 | I32 -> true | _ -> false)
  | _ -> false

type infer_ctx = {
  env : (node_id, expr) Hashtbl.t;
  unresolved : (node_id, infer_kind) Hashtbl.t;
}

type ty_ctx = {
  func_map : (node_id, ty) Hashtbl.t;
  infer_ctx : infer_ctx;
}

type ty_err =
  | MismatchTy of ty * ty
  | MismatchInfer of ty * infer_kind

exception TypeError of ty_err

let ty_err_emit ty_err =
  match ty_err with
  | MismatchTy (expected, ty) ->
      Printf.printf "\x1b[31;1merror\x1b[0m: expected `%s`, found `%s`\n"
        (render_ty expected) (render_ty ty)
  | MismatchInfer (expected, infered_ty) ->
      Printf.printf "\x1b[31;1merror\x1b[0m: expected `%s`, found `%s`\n"
        (render_ty expected)
        (display_infer_kind infered_ty)

let ty_ctx_create () =
  {
    func_map = Hashtbl.create 0;
    infer_ctx = { env = Hashtbl.create 0; unresolved = Hashtbl.create 0 };
  }

let infer (ty_ctx : ty_ctx) (expr : expr) : infer_kind =
  Hashtbl.add ty_ctx.infer_ctx.env expr.expr_id expr;
  let ty =
    match expr.expr_kind with
    | Lit lit -> (
      match lit with
      | LitInt _ -> Int expr.expr_id
      | LitBool _ -> Normal (Prim Bool))
  in
  (match ty with
  | Int id | Float id -> Hashtbl.add ty_ctx.infer_ctx.unresolved id ty
  | _ -> ());
  ty

let infer_block ty_ctx block : infer_kind =
  let f stmt : infer_kind =
    match stmt with
    | Stmt expr | Expr expr -> infer ty_ctx expr
    | Binding _ -> Normal Unit
  in
  ignore (List.map f block.block_stmts);
  match block.last_expr with
  | Some expr -> infer ty_ctx expr
  | None -> Normal Unit

let tychk_func ty_ctx (func : func) =
  let { fn_sig = { args; ret_ty; _ }; body; func_id; _ } = func in
  let ret_ty = Option.value ret_ty ~default:Unit in
  Hashtbl.add ty_ctx.func_map func_id
    (FnTy (List.map (fun (ty, _) -> ty) args, ret_ty));
  match body with
  | Some body -> (
      let ty = infer_block ty_ctx body in
      match ty with
      | Normal ty ->
          if ret_ty = ty then () else ty_err_emit (MismatchTy (ret_ty, ty))
      | Int id ->
          if Hashtbl.mem ty_ctx.infer_ctx.unresolved id then
            if ty_is_int ret_ty then (
              Hashtbl.remove ty_ctx.infer_ctx.unresolved id;
              let expr = Hashtbl.find ty_ctx.infer_ctx.env id in
              expr.expr_ty <- Some ret_ty)
            else ty_err_emit (MismatchInfer (ret_ty, ty))
          else assert false
      | Float id ->
          if Hashtbl.mem ty_ctx.infer_ctx.unresolved id then () else ())
  | None -> ()

let tychk ty_ctx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> tychk_func ty_ctx func
    | _ -> assert false
  in
  List.iter f modd.items
