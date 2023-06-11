open Ast
open Printf

let render (items : 'a list) (func : 'a -> string) (sep : string) : string =
  String.concat sep (List.map (fun item -> func item) items)

let render_path (path : path) : string = String.concat "::" path.segments

let render_lit (lit : lit) : string =
  match lit with
  | LitInt value -> sprintf "%d" value
  | LitFloat value -> sprintf "%f" value
  | LitBool value -> sprintf "%b" value
  | LitStr value -> sprintf "\"%s\"" value

let rec render_ty (ty : ty) : string =
  match ty with
  | Ptr ty -> sprintf "*%s" (render_ty ty)
  | RefTy ty -> sprintf "&%s" (render_ty ty)
  | Prim ty -> (
    match ty with
    | I8 -> "i8"
    | I16 -> "i16"
    | I32 -> "i32"
    | I64 -> "i64"
    | Isize -> "isize"
    | U8 -> "u8"
    | U16 -> "u16"
    | U32 -> "u32"
    | U64 -> "u64"
    | Usize -> "usize"
    | F32 -> "f32"
    | F64 -> "f64"
    | Bool -> "bool"
    | Str -> "str")
  | Unit -> "()"
  | FnTy (ty_list, ret_ty, is_variadic) ->
      sprintf "fn(%s%s) -> %s"
        (render ty_list (fun ty -> render_ty ty) ", ")
        (if is_variadic then ", ..." else "")
        (render_ty ret_ty)

let render_fn_sig (fn_sig : fn_sig) : string =
  sprintf "fn %s(%s)%s" fn_sig.name
    (render fn_sig.args
       (fun (ty, name) -> sprintf "%s%s" (name ^ ": ") (render_ty ty))
       ", ")
    (match fn_sig.ret_ty with
    | Some ty -> " -> " ^ render_ty ty
    | None -> "")

let render_pat pat = match pat with PatIdent ident -> ident

let rec render_expr (expr : expr) : string =
  match expr.expr_kind with
  | Path path -> render_path path
  | Call (path, exprs) ->
      sprintf "%s(%s)" (render_path path) (render exprs render_expr ", ")
  | Lit lit -> render_lit lit
  | Binary (kind, left, right) ->
      sprintf "%s %s %s" (render_expr left)
        (match kind with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Eq -> "=="
        | NotEq -> "!="
        | Gt -> ">"
        | GtEq -> ">="
        | Lt -> "<"
        | LtEq -> "<=")
        (render_expr right)
  | If { cond; then_block; _ } ->
      sprintf "if %s %s" (render_expr cond) (render_block then_block 1)
  | Block block -> render_block block 1
  | _ -> assert false

and render_stmt stmt =
  match stmt with
  | Binding { binding_pat; binding_ty; binding_expr; _ } ->
      sprintf "let %s%s = %s;" (render_pat binding_pat)
        (match binding_ty with
        | Some ty -> sprintf ": %s " (render_ty ty)
        | None -> "")
        (render_expr binding_expr)
  | Assign (left, right) ->
      sprintf "%s = %s;" (render_expr left) (render_expr right)
  | Stmt expr -> render_expr expr ^ ";"
  | Expr expr -> render_expr expr

and render_block (block : block) indent : string =
  let indent = String.make (indent * 4) ' ' in
  sprintf "{\n%s\n%s\n}"
    (render block.block_stmts (fun s -> indent ^ render_stmt s) "\n")
    (match block.last_expr with
    | Some expr -> indent ^ render_expr expr
    | None -> "")

let render_fn (func : func) : string =
  sprintf
    (if func.is_extern then "extern %s%s" else "%s%s")
    (render_fn_sig func.fn_sig)
    (match func.body with
    | Some body -> " " ^ render_block body 1
    | None -> ";")

let render_attr (attr : attr) : string =
  (match (attr.kind, attr.style) with
  | NormalAttr attr, Outer -> sprintf "[%s]" attr.name
  | NormalAttr attr, Inner -> sprintf "![%s]" attr.name
  | Doc doc, _ -> doc)
  ^ "\n"

let render_const (constant : constant) : string = constant.const_name

let render_item (item : item) : string =
  match item with
  | Fn (func, attrs) ->
      sprintf "%s%s\n"
        (render attrs (fun attr -> render_attr attr) "")
        (render_fn func)
  | Const constant -> render_const constant
  | Import path -> sprintf "import %s;\n" (String.concat "::" path.segments)

let render_mod modd : string =
  let rendered_items = render modd.items (fun item -> render_item item) "" in
  if List.length modd.attrs > 0 then
    sprintf "%s\n%s"
      (render modd.attrs (fun attr -> render_attr attr) "")
      rendered_items
  else rendered_items

(* debug utils *)

let display_lit (lit : lit) : string =
  match lit with
  | LitInt value -> sprintf "%d" value
  | LitFloat value -> sprintf "%f" value
  | LitBool value -> sprintf "%b" value
  | LitStr value -> sprintf "%s" value

let rec display_expr_kind (expr_kind : expr_kind) =
  match expr_kind with
  | Lit lit -> "Lit " ^ display_lit lit
  | Path path -> "Path " ^ render_path path
  | Call (path, exprs) ->
      "Call " ^ render_path path ^ ", args: "
      ^ String.concat "\n" (List.map (fun expr -> display_expr expr) exprs)
  | Binary (_, left, right) ->
      "Binary left = " ^ display_expr left ^ ", right = "
      ^ display_expr right
  | If { cond; _ } -> "If cond = " ^ display_expr cond
  | Deref expr -> "Deref expr = " ^ display_expr expr
  | Ref expr -> "Ref expr = " ^ display_expr expr
  | _ -> assert false

and display_expr (expr : expr) =
  sprintf "{ id: %d, ty: %s, kind: %s }" expr.expr_id
    (match expr.expr_ty with Some ty -> render_ty ty | None -> "<none>")
    (display_expr_kind expr.expr_kind)

let display_pat = function PatIdent ident -> ident

let display_stmt (stmt : stmt) indent =
  String.make indent ' '
  ^
  match stmt with
  | Stmt expr | Expr expr -> display_expr expr
  | Binding { binding_pat; binding_ty; binding_expr; binding_id } ->
      sprintf "{ id: %d, pat: %s, ty: %s, expr: %s }" binding_id
        (display_pat binding_pat)
        (match binding_ty with Some ty -> render_ty ty | None -> "<none>")
        (display_expr binding_expr)
  | Assign (lhs, rhs) ->
      "Assign lhs = " ^ display_expr lhs ^ ", init = " ^ display_expr rhs

let display_item (item : item) =
  match item with
  | Fn (func, _) ->
      let { fn_sig = { args; ret_ty; _ }; body; func_id; _ } = func in
      sprintf "func_id:%d\n  args: %s\n  ret_ty: %s\n  body: %s" func_id
        (render args (fun (ty, _) -> render_ty ty) ", ")
        (render_ty (Option.value ret_ty ~default:Unit))
        (match body with
        | Some block -> (
            render block.block_stmts (fun stmt -> display_stmt stmt 4) "\n"
            ^ "\n"
            ^
            match block.last_expr with
            | Some expr -> "    " ^ display_expr expr
            | None -> "    last_expr: None")
        | None -> "<none>")
  | _ -> ""

let display_mod (modd : modd) =
  render modd.items (fun item -> display_item item) "\n"
