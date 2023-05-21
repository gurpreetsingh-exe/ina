open Ast
open Token
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
  | Prim ty -> (
    match ty with
    | I64 | I32 | I16 | I8 | Isize | U64 | U32 | U16 | U8 | Usize -> true
    | _ -> false)
  | _ -> false

let ty_is_float (ty : ty) : bool =
  match ty with
  | Prim ty -> ( match ty with F32 | F64 -> true | _ -> false)
  | _ -> false

type infer_ctx = {
  mutable ty_env : env;
  env : (node_id, expr) Hashtbl.t;
  unresolved : (node_id, infer_kind) Hashtbl.t;
  func_map : (ident, ty) Hashtbl.t;
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
  | FnNotFound of ident
  | MismatchArgs of ident * int * int

let error = ref 0

let infer_err_emit (ty_err : infer_err) (span : span) =
  incr error;
  match ty_err with
  | MismatchInfer (expected, infered_ty) ->
      Printf.printf "\x1b[31;1m%s\x1b[0m: expected `%s`, found `%s`\n"
        (display_span span) (render_ty expected)
        (display_infer_kind infered_ty)
  | MismatchTy (expected, ty) ->
      Printf.printf "\x1b[31;1m%s\x1b[0m: expected `%s`, found `%s`\n"
        (display_span span) (render_ty expected) (render_ty ty)
  | FnNotFound ident ->
      Printf.printf "\x1b[31;1m%s\x1b[0m: function `%s` is not defined\n"
        (display_span span) ident
  | MismatchArgs (func, expected, args) ->
      Printf.printf "\x1b[31;1m%s\x1b[0m: `%s` expected %d arg%s, found %d\n"
        (display_span span) func expected
        (if expected = 1 then "" else "s")
        args

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

let resolve (infer_ctx : infer_ctx) (node_id : node_id)
    (infer_kind : infer_kind) =
  let f id else_ty =
    if bindings_id_exists infer_ctx.ty_env id then (
      let name = find_binding_id infer_ctx.ty_env id in
      match find_ty infer_ctx.ty_env name with
      | Normal ty -> ty
      | _ -> else_ty)
    else else_ty
  in
  let ty =
    match infer_kind with
    | Int id -> f id (Prim I64)
    | Float id -> f id (Prim F32)
    | Normal _ -> assert false
  in
  let expr = Hashtbl.find infer_ctx.env node_id in
  expr.expr_ty <- Some ty

let rec infer (infer_ctx : infer_ctx) (expr : expr) : infer_kind =
  Hashtbl.add infer_ctx.env expr.expr_id expr;
  let ty =
    match expr.expr_kind with
    | Lit lit -> (
      match lit with
      | LitInt _ -> Int expr.expr_id
      | LitFloat _ -> Float expr.expr_id
      | LitBool _ -> Normal (Prim Bool)
      | LitStr _ -> Normal (Prim Str))
    | Ident ident -> (
        Hashtbl.add infer_ctx.ty_env.bindings_id expr.expr_id ident;
        let ty = find_ty infer_ctx.ty_env ident in
        match ty with
        | Int _ -> Int expr.expr_id
        | Float _ -> Float expr.expr_id
        | ty -> ty)
    | Call (ident, exprs) ->
        if Hashtbl.mem infer_ctx.func_map ident then (
          let func = Hashtbl.find infer_ctx.func_map ident in
          let check (expr : expr) (expected : ty) =
            let ty = infer infer_ctx expr in
            match unify infer_ctx ty expected with
            | Some err -> infer_err_emit err expr.expr_span
            | None -> ()
          in
          match func with
          | FnTy (args_ty, ret_ty, is_variadic) ->
              let expr_len = List.length exprs in
              let ty_len = List.length args_ty in
              if is_variadic then
                for i = 0 to expr_len - 1 do
                  if i < ty_len then
                    check (List.nth exprs i) (List.nth args_ty i)
                  else ignore (infer infer_ctx (List.nth exprs i))
                done
              else if expr_len <> ty_len then
                infer_err_emit
                  (MismatchArgs (ident, ty_len, expr_len))
                  expr.expr_span
              else List.iter2 (fun expr ty -> check expr ty) exprs args_ty;
              Normal ret_ty
          | _ -> assert false)
        else (
          infer_err_emit (FnNotFound ident) expr.expr_span;
          Normal Unit)
  in
  (match ty with
  | Int _ | Float _ -> Hashtbl.add infer_ctx.unresolved expr.expr_id ty
  | Normal ty -> expr.expr_ty <- Some ty);
  ty

and unify (infer_ctx : infer_ctx) (ty : infer_kind) (expected : ty) :
    infer_err option =
  let f (check_ty : ty -> bool) (id : node_id) =
    let expr = Hashtbl.find infer_ctx.env id in
    if Hashtbl.mem infer_ctx.unresolved id then
      if check_ty expected then (
        Hashtbl.remove infer_ctx.unresolved id;
        if bindings_id_exists infer_ctx.ty_env id then (
          let binding = find_binding_id infer_ctx.ty_env id in
          if ty_exists infer_ctx.ty_env binding then (
            let ty = find_ty infer_ctx.ty_env binding in
            ignore (unify infer_ctx ty expected);
            Hashtbl.replace infer_ctx.ty_env.bindings binding
              (Normal expected)));
        expr.expr_ty <- Some expected;
        None)
      else Some (MismatchInfer (expected, ty))
    else assert false
  in
  match ty with
  | Normal ty ->
      if ty <> expected then Some (MismatchTy (expected, ty)) else None
  | Int id -> f ty_is_int id
  | Float id -> f ty_is_float id

let infer_block (infer_ctx : infer_ctx) (block : block) : infer_kind =
  let f stmt : infer_kind =
    match stmt with
    | Stmt expr | Expr expr -> infer infer_ctx expr
    | Binding { binding_pat; binding_ty; binding_expr; _ } ->
        (let ty = infer infer_ctx binding_expr in
         match binding_pat with
         | PatIdent ident -> (
           match binding_ty with
           | Some expected ->
               Hashtbl.add infer_ctx.ty_env.bindings ident (Normal expected);
               ignore (unify infer_ctx ty expected)
           | None -> Hashtbl.add infer_ctx.ty_env.bindings ident ty));
        Normal Unit
  in
  ignore (List.map f block.block_stmts);
  let ty =
    match block.last_expr with
    | Some expr -> infer infer_ctx expr
    | None -> Normal Unit
  in
  ty

let infer_func (infer_ctx : infer_ctx) (func : func) =
  let { fn_sig = { name; args; ret_ty; fn_span; is_variadic }; body; _ } =
    func
  in
  let ret_ty = Option.value ret_ty ~default:Unit in
  Hashtbl.add infer_ctx.func_map name
    (FnTy (List.map (fun (ty, _) -> ty) args, ret_ty, is_variadic));
  match body with
  | Some body ->
      let tmp_env = infer_ctx.ty_env in
      infer_ctx.ty_env <- env_create (Some tmp_env);
      List.iter
        (fun (ty, ident) ->
          Hashtbl.add infer_ctx.ty_env.bindings ident (Normal ty))
        args;
      let ty = infer_block infer_ctx body in
      (match (unify infer_ctx ty ret_ty, body.last_expr) with
      | Some err, Some expr -> infer_err_emit err expr.expr_span
      | None, (None | _) -> ()
      | Some err, None -> infer_err_emit err fn_span);
      Hashtbl.iter (resolve infer_ctx) infer_ctx.unresolved;
      infer_ctx.ty_env <- tmp_env
  | None -> ()

let infer_begin infer_ctx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> infer_func infer_ctx func
    | _ -> assert false
  in
  List.iter f modd.items
