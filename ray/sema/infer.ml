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

type env = {
  parent : env option;
  bindings : (string, infer_kind) Hashtbl.t;
}

let env_create parent : env = { parent; bindings = Hashtbl.create 0 }

let ty_is_int (ty : ty) : bool =
  match ty with
  | Prim ty -> ( match ty with I64 | I32 -> true | _ -> false)
  | _ -> false

type infer_ctx = {
  ty_env : env;
  env : (node_id, expr) Hashtbl.t;
  unresolved : (node_id, infer_kind) Hashtbl.t;
  func_map : (node_id, ty) Hashtbl.t;
}

let infer_ctx_create () =
  {
    ty_env = env_create None;
    env = Hashtbl.create 0;
    unresolved = Hashtbl.create 0;
    func_map = Hashtbl.create 0;
  }

type infer_err =
  | MismatchInfer of ty * infer_kind
  | MismatchTy of ty * ty

let infer_err_emit (ty_err : infer_err) =
  match ty_err with
  | MismatchInfer (expected, infered_ty) ->
      Printf.printf "\x1b[31;1merror\x1b[0m: expected `%s`, found `%s`\n"
        (render_ty expected)
        (display_infer_kind infered_ty)
  | MismatchTy (expected, ty) ->
      Printf.printf "\x1b[31;1merror\x1b[0m: expected `%s`, found `%s`\n"
        (render_ty expected) (render_ty ty)

let infer (infer_ctx : infer_ctx) (expr : expr) : infer_kind =
  Hashtbl.add infer_ctx.env expr.expr_id expr;
  let ty =
    match expr.expr_kind with
    | Lit lit -> (
      match lit with
      | LitInt _ -> Int expr.expr_id
      | LitBool _ -> Normal (Prim Bool))
  in
  (match ty with
  | Int id | Float id -> Hashtbl.add infer_ctx.unresolved id ty
  | Normal ty -> expr.expr_ty <- Some ty);
  ty

let unify infer_ctx ty expected =
  match ty with
  | Normal _ -> ()
  | Int id ->
      if Hashtbl.mem infer_ctx.unresolved id then
        if ty_is_int expected then (
          Hashtbl.remove infer_ctx.unresolved id;
          let expr = Hashtbl.find infer_ctx.env id in
          expr.expr_ty <- Some expected)
        else infer_err_emit (MismatchInfer (expected, ty))
      else assert false
  | Float id ->
      ignore (assert false);
      if Hashtbl.mem infer_ctx.unresolved id then () else assert false

let infer_block infer_ctx block : infer_kind =
  let f stmt : infer_kind =
    match stmt with
    | Stmt expr | Expr expr -> infer infer_ctx expr
    | Binding { binding_pat; binding_ty; binding_expr; _ } ->
        (let ty = infer infer_ctx binding_expr in
         match binding_pat with
         | PatIdent ident -> (
             Hashtbl.add infer_ctx.ty_env.bindings ident ty;
             match binding_ty with
             | Some expected -> unify infer_ctx ty expected
             | None -> ()));
        Normal Unit
  in
  ignore (List.map f block.block_stmts);
  match block.last_expr with
  | Some expr -> infer infer_ctx expr
  | None -> Normal Unit

let resolve infer_ctx node_id infer_kind =
  let ty =
    match infer_kind with
    | Int _ -> Prim I64
    | Float _ -> Prim F32
    | Normal _ -> assert false
  in
  let expr = Hashtbl.find infer_ctx.env node_id in
  expr.expr_ty <- Some ty

let infer_func infer_ctx (func : func) =
  let { fn_sig = { args; ret_ty; _ }; body; func_id; _ } = func in
  let ret_ty = Option.value ret_ty ~default:Unit in
  Hashtbl.add infer_ctx.func_map func_id
    (FnTy (List.map (fun (ty, _) -> ty) args, ret_ty));
  (match body with
  | Some body ->
      let ty = infer_block infer_ctx body in
      unify infer_ctx ty ret_ty
  | None -> ());
  Hashtbl.iter (resolve infer_ctx) infer_ctx.unresolved

let infer_begin infer_ctx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> infer_func infer_ctx func
    | _ -> assert false
  in
  List.iter f modd.items
