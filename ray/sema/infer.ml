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

let debug_display_infer_kind = function
  | Int id -> Printf.sprintf "int %d" id
  | Float id -> Printf.sprintf "float %d" id
  | Normal ty -> render_ty ty

type env = {
  parent : env option;
  bindings : (string, infer_kind) Hashtbl.t;
  bindings_id : (node_id, string) Hashtbl.t;
}

let env_create parent : env =
  { parent; bindings = Hashtbl.create 0; bindings_id = Hashtbl.create 0 }

let rec ty_exists (ty_env : env) ident : bool =
  if not (Hashtbl.mem ty_env.bindings ident) then (
    match ty_env.parent with
    | Some env -> ty_exists env ident
    | None -> false)
  else true

let rec find_ty (ty_env : env) ident : infer_kind =
  if Hashtbl.mem ty_env.bindings ident then
    Hashtbl.find ty_env.bindings ident
  else (
    match ty_env.parent with
    | Some env -> find_ty env ident
    | None -> assert false)

let rec bindings_id_exists (ty_env : env) id : bool =
  if not (Hashtbl.mem ty_env.bindings_id id) then (
    match ty_env.parent with
    | Some env -> bindings_id_exists env id
    | None -> false)
  else true

let rec find_binding_id (ty_env : env) id : string =
  if Hashtbl.mem ty_env.bindings_id id then
    Hashtbl.find ty_env.bindings_id id
  else (
    match ty_env.parent with
    | Some env -> find_binding_id env id
    | None -> assert false)

let ty_is_int (ty : ty) : bool =
  match ty with
  | Prim ty -> ( match ty with I64 | I32 -> true | _ -> false)
  | _ -> false

type infer_ctx = {
  mutable ty_env : env;
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

let print_unresolved unresolved =
  Hashtbl.iter
    (fun key value ->
      Printf.printf "%d: %s\n" key (display_infer_kind value))
    unresolved

let rec print_bindings ty_env =
  Hashtbl.iter
    (fun key value ->
      Printf.printf "%s: %s\n" key (display_infer_kind value))
    ty_env.bindings;
  Hashtbl.iter
    (fun key value -> Printf.printf "%d: %s\n" key value)
    ty_env.bindings_id;
  match ty_env.parent with Some env -> print_bindings env | None -> ()

let infer (infer_ctx : infer_ctx) (expr : expr) : infer_kind =
  Hashtbl.add infer_ctx.env expr.expr_id expr;
  let ty =
    match expr.expr_kind with
    | Lit lit -> (
      match lit with
      | LitInt _ -> Int expr.expr_id
      | LitBool _ -> Normal (Prim Bool))
    | Ident ident -> (
        Hashtbl.add infer_ctx.ty_env.bindings_id expr.expr_id ident;
        let ty = find_ty infer_ctx.ty_env ident in
        match ty with
        | Int _ -> Int expr.expr_id
        | Float _ -> Float expr.expr_id
        | ty -> ty)
  in
  (match ty with
  | Int _ | Float _ -> Hashtbl.add infer_ctx.unresolved expr.expr_id ty
  | Normal ty -> expr.expr_ty <- Some ty);
  ty

let rec unify infer_ctx ty expected =
  match ty with
  | Normal ty ->
      if ty <> expected then infer_err_emit (MismatchTy (expected, ty))
  | Int id ->
      if Hashtbl.mem infer_ctx.unresolved id then
        if ty_is_int expected then (
          Hashtbl.remove infer_ctx.unresolved id;
          let expr = Hashtbl.find infer_ctx.env id in
          if bindings_id_exists infer_ctx.ty_env id then (
            let binding = find_binding_id infer_ctx.ty_env id in
            if ty_exists infer_ctx.ty_env binding then (
              let ty = find_ty infer_ctx.ty_env binding in
              unify infer_ctx ty expected;
              Hashtbl.replace infer_ctx.ty_env.bindings binding
                (Normal expected)));
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
           match binding_ty with
           | Some expected ->
               if not (Hashtbl.mem infer_ctx.ty_env.bindings ident) then
                 Hashtbl.add infer_ctx.ty_env.bindings ident
                   (Normal expected);
               unify infer_ctx ty expected
           | None ->
               if not (Hashtbl.mem infer_ctx.ty_env.bindings ident) then
                 Hashtbl.add infer_ctx.ty_env.bindings ident ty));
        Normal Unit
  in
  ignore (List.map f block.block_stmts);
  let ty =
    match block.last_expr with
    | Some expr -> infer infer_ctx expr
    | None -> Normal Unit
  in
  ty

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
      let tmp_env = infer_ctx.ty_env in
      infer_ctx.ty_env <- env_create (Some tmp_env);
      List.iter
        (fun (ty, ident) ->
          Hashtbl.add infer_ctx.ty_env.bindings ident (Normal ty))
        args;
      let ty = infer_block infer_ctx body in
      unify infer_ctx ty ret_ty;
      infer_ctx.ty_env <- tmp_env
  | None -> ());
  Hashtbl.iter (resolve infer_ctx) infer_ctx.unresolved

let infer_begin infer_ctx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> infer_func infer_ctx func
    | _ -> assert false
  in
  List.iter f modd.items
