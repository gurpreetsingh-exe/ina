open Ast
open Ty
open Token
open Front.Fmt
open Printf

type env = {
  parent : env option;
  bindings : (string, ty) Hashtbl.t;
}

let env_create parent : env = { parent; bindings = Hashtbl.create 0 }

let rec find_ty (ty_env : env) ident : ty =
  if Hashtbl.mem ty_env.bindings ident then
    Hashtbl.find ty_env.bindings ident
  else (
    match ty_env.parent with
    | Some env -> find_ty env ident
    | None -> assert false)

let ty_is_int (ty : ty) : bool = match ty with Int _ -> true | _ -> false

let ty_is_float (ty : ty) : bool =
  match ty with Float _ -> true | _ -> false

let fn_ty func =
  let { fn_sig = { args; ret_ty; is_variadic; _ }; _ } = func in
  let ret_ty = Option.value ret_ty ~default:Unit in
  FnTy (List.map (fun (ty, _) -> ty) args, ret_ty, is_variadic)

type infer_ctx = {
  mutable ty_env : env;
  env : (node_id, expr) Hashtbl.t;
  int_unifiction_table : (ty, ty) Hashtbl.t;
  int_ut : (ty_vid, ty option) Unification_table.t;
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
    int_unifiction_table = Hashtbl.create 0;
    int_ut = { values = [||] };
    func_map = Hashtbl.create 0;
    globl_env;
  }

let equate infer_ctx t0 t1 =
  if not (Hashtbl.mem infer_ctx.int_unifiction_table t0) then
    Hashtbl.add infer_ctx.int_unifiction_table t0 t1;
  if not (Hashtbl.mem infer_ctx.int_unifiction_table t1) then
    Hashtbl.add infer_ctx.int_unifiction_table t1 t0;
  Hashtbl.iter
    (fun k v ->
      try
        let found_v = Hashtbl.find infer_ctx.int_unifiction_table k in
        let found_k = Hashtbl.find infer_ctx.int_unifiction_table v in
        if found_v = v && found_k = k then
          Hashtbl.remove infer_ctx.int_unifiction_table v
      with Not_found -> ())
    infer_ctx.int_unifiction_table

let print_uf_tbl infer_ctx =
  printf "{%s }\n"
    (String.concat "\n"
       (List.of_seq
          (Seq.map
             (fun (k, v) -> sprintf " %s -> %s" (render_ty k) (render_ty v))
             (Hashtbl.to_seq infer_ctx.int_unifiction_table))))

type infer_err =
  | MismatchTy of ty * ty
  | FnNotFound of ident
  | MismatchArgs of ident * int * int
  | InvalidDeref of ty

let error = ref 0

let infer_err_emit (ty_err : infer_err) (span : span) =
  incr error;
  match ty_err with
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
  | InvalidDeref ty ->
      Printf.printf "\x1b[31;1m%s\x1b[0m: `%s` cannot be dereferenced\n"
        (display_span span) (render_ty ty)

let rec print_bindings ty_env =
  Hashtbl.iter
    (fun key value -> Printf.printf "%s: %s\n" key (render_ty value))
    ty_env.bindings;
  print_endline "======================";
  match ty_env.parent with Some env -> print_bindings env | None -> ()

let rec resolve_block (infer_ctx : infer_ctx) body =
  let rec sub ty =
    match ty with
    | Infer ((IntVar _ | FloatVar _) as i) ->
        sub
          (if Hashtbl.mem infer_ctx.int_unifiction_table ty then
           Hashtbl.find infer_ctx.int_unifiction_table ty
          else (
            match i with
            | IntVar _ -> Int I64
            | FloatVar _ -> Float F32
            | _ -> assert false))
    | RefTy ty -> RefTy (sub ty)
    | Ptr ty -> Ptr (sub ty)
    | _ -> ty
  in
  let rec f stmt =
    match stmt with
    | Stmt expr | Expr expr -> g expr
    | Binding { binding_expr; _ } -> g binding_expr
    | Assign (l, init) -> g l; g init
  and g expr =
    let ty = Option.get expr.expr_ty in
    expr.expr_ty <- Some (sub ty);
    match expr.expr_kind with
    | Binary (_, left, right) -> g left; g right
    | Block block -> resolve_block infer_ctx block
    | Call (_, exprs) -> List.iter g exprs
    | Deref expr | Ref expr -> g expr
    | If { cond; then_block; else_block } -> (
        g cond;
        resolve_block infer_ctx then_block;
        match else_block with Some e -> g e | None -> ())
    | _ -> ()
  in
  List.iter f body.block_stmts;
  match body.last_expr with Some e -> g e | None -> ()

let rec infer (infer_ctx : infer_ctx) (expr : expr) : ty =
  Hashtbl.add infer_ctx.env expr.expr_id expr;
  let ty =
    match expr.expr_kind with
    | Lit lit -> (
      match lit with
      | LitInt _ ->
          Infer (IntVar (Unification_table.new_key infer_ctx.int_ut None))
      | LitFloat _ -> Infer (FloatVar { index = float_var_id () })
      | LitBool _ -> Bool
      | LitStr _ -> Str)
    | Path path ->
        let ident = render_path path in
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
              ret_ty
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
          Unit)
    | Binary (kind, left, right) -> (
        let left, right = (infer infer_ctx left, infer infer_ctx right) in
        equate infer_ctx left right;
        let cmp t1 : ty = match kind with Eq | NotEq -> Bool | _ -> t1 in
        (* TODO: move all then non infer stuff to tychk *)
        match (left, right) with
        | Infer (IntVar t0), Infer (IntVar t1) ->
            cmp (Infer (IntVar { index = min t0.index t1.index }))
        | Infer (FloatVar _), Infer (FloatVar _) ->
            cmp (Infer (FloatVar { index = expr.expr_id }))
        | Infer (IntVar _), Infer (FloatVar _)
         |Infer (FloatVar _), Infer (IntVar _)
         |Infer (IntVar _ | FloatVar _), (RefTy _ | Ptr _)
         |(RefTy _ | Ptr _), Infer (IntVar _ | FloatVar _) ->
            infer_err_emit (MismatchTy (left, right)) expr.expr_span;
            cmp left
        | t0, Infer (IntVar _ | FloatVar _) ->
            ignore (unify infer_ctx right t0);
            cmp left
        | Infer (IntVar _ | FloatVar _), t0 ->
            ignore (unify infer_ctx left t0);
            cmp right
        | t0, t1 ->
            if t0 <> t1 then
              infer_err_emit (MismatchTy (left, right)) expr.expr_span;
            cmp left)
    | If { cond; then_block; else_block } -> (
        let cond_ty = infer infer_ctx cond in
        (match unify infer_ctx cond_ty Bool with
        | Some err -> infer_err_emit err cond.expr_span
        | None -> ());
        let then_ty = infer_block infer_ctx then_block in
        match else_block with
        | Some elze -> (
            let else_ty = infer infer_ctx elze in
            equate infer_ctx then_ty else_ty;
            match (then_ty, else_ty) with
            | Infer (IntVar _), Infer (IntVar _) ->
                Infer (IntVar { index = expr.expr_id })
            | Infer (FloatVar _), Infer (FloatVar _) ->
                Infer (FloatVar { index = expr.expr_id })
            | Infer (IntVar _), Infer (FloatVar _)
             |Infer (FloatVar _), Infer (IntVar _) ->
                infer_err_emit (MismatchTy (then_ty, else_ty)) expr.expr_span;
                then_ty
            | t0, Infer (IntVar _ | FloatVar _) ->
                ignore (unify infer_ctx else_ty t0);
                then_ty
            | Infer (IntVar _ | FloatVar _), t0 ->
                ignore (unify infer_ctx then_ty t0);
                else_ty
            | t0, t1 ->
                if t0 <> t1 then
                  infer_err_emit
                    (MismatchTy (then_ty, else_ty))
                    expr.expr_span;
                then_ty)
        | None -> Unit)
    | Block block -> infer_block infer_ctx block
    | Deref expr -> (
      match infer infer_ctx expr with
      | Ptr ty | RefTy ty -> ty
      | ty ->
          infer_err_emit (InvalidDeref ty) expr.expr_span;
          ty)
    | Ref expr -> RefTy (infer infer_ctx expr)
  in
  (match ty with
  | Infer (IntVar _ | FloatVar _) -> expr.expr_ty <- Some ty
  | ty -> expr.expr_ty <- Some ty);
  ty

and rec_replace (infer_ctx : infer_ctx) k ty =
  if Hashtbl.mem infer_ctx.int_unifiction_table k then (
    let vty = Hashtbl.find infer_ctx.int_unifiction_table k in
    Hashtbl.replace infer_ctx.int_unifiction_table k ty;
    rec_replace infer_ctx vty ty)
  else Hashtbl.add infer_ctx.int_unifiction_table k ty

and unify (infer_ctx : infer_ctx) (ty : ty) (expected : ty) :
    infer_err option =
  let f (check_ty : ty -> bool) (ty_kind : ty) =
    if check_ty expected then (
      rec_replace infer_ctx ty_kind expected;
      None)
    else Some (MismatchTy (expected, ty_kind))
  in
  match ty with
  | Infer (IntVar _) -> f ty_is_int ty
  | Infer (FloatVar _) -> f ty_is_float ty
  | ty -> if ty <> expected then Some (MismatchTy (expected, ty)) else None

and infer_block (infer_ctx : infer_ctx) (block : block) : ty =
  let f stmt : ty =
    match stmt with
    | Stmt expr | Expr expr -> infer infer_ctx expr
    | Binding { binding_pat; binding_ty; binding_expr; _ } ->
        (let ty = infer infer_ctx binding_expr in
         match binding_pat with
         | PatIdent ident -> (
           match binding_ty with
           | Some expected ->
               add_binding infer_ctx ident expected;
               ignore (unify infer_ctx ty expected)
           | None -> add_binding infer_ctx ident ty));
        Unit
    | Assign (l, init) ->
        let lty = infer infer_ctx l in
        let rty = infer infer_ctx init in
        equate infer_ctx lty rty;
        let f ty exp =
          match unify infer_ctx exp ty with
          | Some err -> infer_err_emit err init.expr_span
          | None -> ()
        in
        (match (lty, rty) with
        | Infer (IntVar _), Infer (IntVar _)
         |Infer (FloatVar _), Infer (FloatVar _) ->
            ()
        | Infer (IntVar _), Infer (FloatVar _)
         |Infer (FloatVar _), Infer (IntVar _) ->
            infer_err_emit (MismatchTy (lty, rty)) init.expr_span
        | ty, Infer (IntVar _ | FloatVar _) -> f ty rty
        | Infer (IntVar _ | FloatVar _), ty -> f ty lty
        | l, r ->
            if l <> r then
              infer_err_emit (MismatchTy (lty, rty)) init.expr_span);
        Unit
  in
  let tmp_env = infer_ctx.ty_env in
  infer_ctx.ty_env <- env_create (Some tmp_env);
  ignore (List.map f block.block_stmts);
  let ty =
    match block.last_expr with
    | Some expr -> infer infer_ctx expr
    | None -> Unit
  in
  infer_ctx.ty_env <- tmp_env;
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
        (fun (ty, ident) -> Hashtbl.add infer_ctx.ty_env.bindings ident ty)
        args;
      let ty = infer_block infer_ctx body in
      (match (unify infer_ctx ty ret_ty, body.last_expr) with
      | Some err, Some expr -> infer_err_emit err expr.expr_span
      | None, (None | _) -> ()
      | Some err, None -> infer_err_emit err fn_span);
      resolve_block infer_ctx body;
      infer_ctx.ty_env <- tmp_env
  | None -> ()

let infer_begin infer_ctx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> infer_func infer_ctx func
    | Import _ -> ()
    | Foreign funcs -> List.iter (fun f -> infer_func infer_ctx f) funcs
    | _ -> assert false
  in
  List.iter f modd.items
