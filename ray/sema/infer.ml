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
  unresolved : (node_id, infer_kind) Hashtbl.t;
  bindings : (string, infer_kind) Hashtbl.t;
  bindings_id : (node_id, string) Hashtbl.t;
  expr_paren : (node_id, node_id) Hashtbl.t;
}

let env_create parent : env =
  {
    parent;
    unresolved = Hashtbl.create 0;
    bindings = Hashtbl.create 0;
    bindings_id = Hashtbl.create 0;
    expr_paren = Hashtbl.create 0;
  }

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

let fn_ty func =
  let { fn_sig = { args; ret_ty; is_variadic; _ }; _ } = func in
  let ret_ty = Option.value ret_ty ~default:Unit in
  FnTy (List.map (fun (ty, _) -> ty) args, ret_ty, is_variadic)

type infer_ctx = {
  mutable ty_env : env;
  env : (node_id, expr) Hashtbl.t;
  globl_env : (path, lang_item) Hashtbl.t;
  func_map : (ident, ty) Hashtbl.t;
}

let add_binding (infer_ctx : infer_ctx) ident ty =
  if Hashtbl.mem infer_ctx.ty_env.bindings ident then
    Hashtbl.replace infer_ctx.ty_env.bindings ident ty
  else Hashtbl.add infer_ctx.ty_env.bindings ident ty

let infer_ctx_create globl_env =
  {
    ty_env = env_create None;
    env = Hashtbl.create 0;
    func_map = Hashtbl.create 0;
    globl_env;
  }

type infer_err =
  | MismatchInfer of ty * infer_kind
  | MismatchTy of infer_kind * infer_kind
  | FnNotFound of ident
  | MismatchArgs of ident * int * int
  | InvalidDeref of infer_kind

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
        (display_span span)
        (display_infer_kind expected)
        (display_infer_kind ty)
  | FnNotFound ident ->
      Printf.printf "\x1b[31;1m%s\x1b[0m: function `%s` is not defined\n"
        (display_span span) ident
  | MismatchArgs (func, expected, args) ->
      Printf.printf "\x1b[31;1m%s\x1b[0m: `%s` expected %d arg%s, found %d\n"
        (display_span span) func expected
        (if expected = 1 then "" else "s")
        args
  | InvalidDeref ty ->
      Printf.printf "\x1b[31;1m%s\x1b[0m: `%s` cannot be dereferenced\n"
        (display_span span) (display_infer_kind ty)

let print_unresolved unresolved =
  Hashtbl.iter
    (fun key value ->
      Printf.printf "%d: %s\n" key (display_infer_kind value))
    unresolved

let rec print_bindings ty_env =
  Hashtbl.iter
    (fun key value ->
      Printf.printf "%s: %s\n" key (debug_display_infer_kind value))
    ty_env.bindings;
  print_endline "======================";
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
    | Path path ->
        let ident = render_path path in
        Hashtbl.add infer_ctx.ty_env.bindings_id expr.expr_id ident;
        find_ty infer_ctx.ty_env ident
    | Call (path, exprs) ->
        let f func ident =
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
          | _ -> assert false
        in
        let ident = render_path path in
        if Hashtbl.mem infer_ctx.func_map ident then (
          let func = Hashtbl.find infer_ctx.func_map ident in
          f func ident)
        else if Hashtbl.mem infer_ctx.globl_env path then (
          match Hashtbl.find infer_ctx.globl_env path with
          | Fn func -> f (fn_ty func) ident
          | _ -> assert false)
        else (
          infer_err_emit (FnNotFound ident) expr.expr_span;
          Normal Unit)
    | Binary (kind, left, right) -> (
        Hashtbl.add infer_ctx.ty_env.expr_paren left.expr_id expr.expr_id;
        Hashtbl.add infer_ctx.ty_env.expr_paren right.expr_id expr.expr_id;
        let left, right = (infer infer_ctx left, infer infer_ctx right) in
        let cmp t1 =
          match kind with Eq | NotEq -> Normal (Prim Bool) | _ -> t1
        in
        match (left, right) with
        | Normal t0, Normal t1 ->
            if t0 <> t1 then
              infer_err_emit (MismatchTy (left, right)) expr.expr_span;
            cmp left
        | Int _, Int _ -> cmp (Int expr.expr_id)
        | Float _, Float _ -> cmp (Float expr.expr_id)
        | Normal t0, (Int _ | Float _) ->
            ignore (unify infer_ctx right t0);
            cmp left
        | (Int _ | Float _), Normal t0 ->
            ignore (unify infer_ctx left t0);
            cmp right
        | a, b ->
            infer_err_emit (MismatchTy (a, b)) expr.expr_span;
            cmp left)
    | If { cond; then_block; else_block } -> (
        let cond_ty = infer infer_ctx cond in
        (match unify infer_ctx cond_ty (Prim Bool) with
        | Some err -> infer_err_emit err cond.expr_span
        | None -> ());
        let then_ty = infer_block infer_ctx then_block in
        match else_block with
        | Some elze -> (
            let else_ty = infer_block infer_ctx elze in
            match (then_ty, else_ty) with
            | Normal t0, Normal t1 ->
                if t0 <> t1 then
                  infer_err_emit
                    (MismatchTy (then_ty, else_ty))
                    expr.expr_span;
                then_ty
            | Int _, Int _ -> Int expr.expr_id
            | Float _, Float _ -> Float expr.expr_id
            | Normal t0, (Int _ | Float _) ->
                ignore (unify infer_ctx else_ty t0);
                then_ty
            | (Int _ | Float _), Normal t0 ->
                ignore (unify infer_ctx then_ty t0);
                else_ty
            | a, b ->
                infer_err_emit (MismatchTy (a, b)) expr.expr_span;
                then_ty)
        | None -> Normal Unit)
    | Block block -> infer_block infer_ctx block
    | Deref expr -> (
      match infer infer_ctx expr with
      | Normal ty -> (
        match ty with
        | Ptr ty | RefTy ty -> Normal ty
        | ty ->
            infer_err_emit (InvalidDeref (Normal ty)) expr.expr_span;
            Normal ty)
      | ty ->
          infer_err_emit (InvalidDeref ty) expr.expr_span;
          ty)
    | Ref expr -> (
      match infer infer_ctx expr with
      | Normal ty -> Normal (RefTy ty)
      | _ -> assert false)
  in
  (match ty with
  | Int _ | Float _ ->
      Hashtbl.add infer_ctx.ty_env.unresolved expr.expr_id ty
  | Normal ty -> expr.expr_ty <- Some ty);
  ty

and unify (infer_ctx : infer_ctx) (ty : infer_kind) (expected : ty) :
    infer_err option =
  let rec set_type expr : infer_err option =
    expr.expr_ty <- Some expected;
    Hashtbl.remove infer_ctx.ty_env.unresolved expr.expr_id;
    match expr.expr_kind with
    | Binary (_, left, right) -> (
      match set_type left with
      | Some err -> Some err
      | None -> set_type right)
    | Path path ->
        let name = render_path path in
        let binding = Hashtbl.find infer_ctx.ty_env.bindings name in
        Hashtbl.replace infer_ctx.ty_env.bindings name (Normal expected);
        unify infer_ctx binding expected
    | Block block -> fblock block
    | If { then_block; else_block; _ } -> (
      match fblock then_block with
      | None -> (
        match else_block with
        | Some else_block -> fblock else_block
        | None -> None)
      | err -> err)
    | _ -> None
  and fblock block =
    match block.last_expr with Some expr -> set_type expr | None -> None
  in
  let f (check_ty : ty -> bool) (ty_kind : infer_kind) =
    if check_ty expected then (
      Hashtbl.iter
        (fun id t ->
          if t = ty then (
            let expr = Hashtbl.find infer_ctx.env id in
            (match set_type expr with
            | Some err -> infer_err_emit err expr.expr_span
            | None -> ());
            if Hashtbl.mem infer_ctx.ty_env.expr_paren id then (
              let p_id = Hashtbl.find infer_ctx.ty_env.expr_paren id in
              let pexpr = Hashtbl.find infer_ctx.env p_id in
              match set_type pexpr with
              | Some err -> infer_err_emit err pexpr.expr_span
              | None -> ());
            try
              let name = Hashtbl.find infer_ctx.ty_env.bindings_id id in
              Hashtbl.replace infer_ctx.ty_env.bindings name
                (Normal expected);
              Hashtbl.remove infer_ctx.ty_env.bindings_id id
            with Not_found -> ()))
        infer_ctx.ty_env.unresolved;
      None)
    else Some (MismatchTy (Normal expected, ty_kind))
  in
  match ty with
  | Normal ty ->
      if ty <> expected then Some (MismatchTy (Normal expected, Normal ty))
      else None
  | Int _ -> f ty_is_int ty
  | Float _ -> f ty_is_float ty

and infer_block (infer_ctx : infer_ctx) (block : block) : infer_kind =
  let f stmt : infer_kind =
    match stmt with
    | Stmt expr | Expr expr -> infer infer_ctx expr
    | Binding { binding_pat; binding_ty; binding_expr; _ } ->
        (let ty = infer infer_ctx binding_expr in
         match binding_pat with
         | PatIdent ident -> (
           match binding_ty with
           | Some expected ->
               add_binding infer_ctx ident (Normal expected);
               ignore (unify infer_ctx ty expected)
           | None -> add_binding infer_ctx ident ty));
        Normal Unit
    | Assign (l, init) ->
        let lty = infer infer_ctx l in
        let rty = infer infer_ctx init in
        let f ty exp =
          match unify infer_ctx exp ty with
          | Some err -> infer_err_emit err init.expr_span
          | None -> ()
        in
        (match (lty, rty) with
        | Normal l, Normal r ->
            if l <> r then
              infer_err_emit (MismatchTy (lty, rty)) init.expr_span
        | Normal ty, (Int _ | Float _) -> f ty rty
        | (Int _ | Float _), Normal ty -> f ty lty
        | Int _, Int _ | Float _, Float _ -> ()
        | Float _, Int _ | Int _, Float _ ->
            infer_err_emit (MismatchTy (lty, rty)) init.expr_span);
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
      Hashtbl.iter (resolve infer_ctx) infer_ctx.ty_env.unresolved;
      infer_ctx.ty_env <- tmp_env
  | None -> ()

let infer_begin infer_ctx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> infer_func infer_ctx func
    | Import _ -> ()
    | _ -> assert false
  in
  List.iter f modd.items
