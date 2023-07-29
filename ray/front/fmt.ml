open Ast
open Printf
open Ty

let render (items : 'a list) (func : 'a -> string) (sep : string) : string =
  String.concat sep (List.map (fun item -> func item) items)

let render_lit (lit : lit) : string =
  match lit with
  | LitInt value -> sprintf "%d" value
  | LitFloat value -> sprintf "%f" value
  | LitBool value -> sprintf "%b" value
  | LitStr value -> sprintf "\"%s\"" (String.escaped value)

let render_fn_sig (fn_sig : fn_sig) : string =
  sprintf "fn %s(%s)%s" fn_sig.name
    (render fn_sig.args
       (fun (ty, name) -> sprintf "%s%s" (name ^ ": ") (render_ty ty))
       ", ")
    (match fn_sig.ret_ty with
    | Some ty -> " -> " ^ render_ty ty
    | None -> "")

let render_pat pat = match pat with PatIdent ident -> ident

let rec render_expr (expr : expr) (indent : int) : string =
  sprintf "(%s: %s)"
    (match expr.expr_kind with
    | Path path -> render_path path
    | Call (path, exprs) ->
        sprintf "%s(%s)" (render_path path)
          (render exprs (fun e -> render_expr e indent) ", ")
    | Lit lit -> render_lit lit
    | Binary (kind, left, right) ->
        sprintf "%s %s %s" (render_expr left indent)
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
          | LtEq -> "<="
          | BitAnd -> "&"
          | BitOr -> "|"
          | And -> "&&"
          | Or -> "||")
          (render_expr right indent)
    | If { cond; then_block; else_block } ->
        sprintf "if %s %s%s" (render_expr cond indent)
          (render_block then_block (indent + 1))
          (match else_block with
          | Some expr -> " else " ^ render_expr expr indent
          | None -> "")
    | Block block -> render_block block (indent + 1)
    | StructExpr { struct_name; fields } ->
        sprintf "%s { %s }" (render_path struct_name)
          (render fields
             (fun (name, expr) ->
               sprintf "%s: %s" name (render_expr expr indent))
             ", ")
    | Field (expr, name) -> sprintf "%s.%s" (render_expr expr indent) name
    | Deref expr -> sprintf "*%s" (render_expr expr indent)
    | Cast (expr, ty) ->
        sprintf "%s as %s" (render_expr expr indent) (render_ty ty)
    | Ref expr -> sprintf "&%s" (render_expr expr indent))
    (match expr.expr_ty with Some ty -> render_ty ty | None -> "none")

and render_stmt stmt indent =
  String.make (indent * 4) ' '
  ^
  match stmt with
  | Binding { binding_pat; binding_ty; binding_expr; _ } ->
      sprintf "let %s%s = %s;" (render_pat binding_pat)
        (match binding_ty with
        | Some ty -> sprintf ": %s" (render_ty ty)
        | None -> "")
        (render_expr binding_expr indent)
  | Assign (left, right) ->
      sprintf "%s = %s;" (render_expr left indent) (render_expr right indent)
  | Stmt expr -> render_expr expr indent ^ ";"
  | Expr expr -> render_expr expr indent
  | Assert (expr, string) ->
      sprintf "assert %s%s;" (render_expr expr indent)
        (match string with
        | Some expr -> ", " ^ render_expr expr indent
        | None -> "")

and render_block (block : block) indent : string =
  let has_last_expr = Option.is_some block.last_expr in
  let render_stmts () =
    render block.block_stmts (fun s -> render_stmt s indent) "\n"
  in
  let last_expr () =
    String.make (indent * 4) ' '
    ^ render_expr (Option.get block.last_expr) indent
  in
  let indent = String.make ((indent - 1) * 4) ' ' in
  match (List.length block.block_stmts, has_last_expr) with
  | 0, false -> "{}"
  | 0, true -> sprintf "{\n%s\n%s}" (last_expr ()) indent
  | _, false -> sprintf "{\n%s\n%s}" (render_stmts ()) indent
  | _, _ -> sprintf "{\n%s\n%s\n%s}" (render_stmts ()) (last_expr ()) indent

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

let render_struct s =
  sprintf "%s = {\n%s\n}" s.ident
    (String.concat ",\n"
       (List.map
          (fun (ty, name) -> sprintf "    %s%s" (name ^ ": ") (render_ty ty))
          s.members))

let render_type (ty : typ) =
  sprintf "\ntype %s\n" (match ty with Struct s -> render_struct s)

let rec render_item (item : item) : string =
  match item with
  | Fn (func, attrs) ->
      sprintf "\n%s%s\n"
        (render attrs (fun attr -> render_attr attr) "")
        (render_fn func)
  | Type ty -> render_type ty
  | Foreign funcs ->
      sprintf "\nextern {\n%s\n}\n"
        (String.concat "\n" (List.map (fun f -> "    " ^ render_fn f) funcs))
  | Const constant -> render_const constant
  | Mod { name; resolved_mod } -> (
    match resolved_mod with
    | Some modd -> sprintf "mod %s {%s}\n" name (render_mod modd)
    | None -> "mod " ^ name ^ ";")
  | Import path -> sprintf "import %s;\n" (String.concat "::" path.segments)

and render_mod modd : string =
  let rendered_items = render modd.items (fun item -> render_item item) "" in
  if List.length modd.attrs > 0 then
    sprintf "%s\n%s"
      (render modd.attrs (fun attr -> render_attr attr) "")
      rendered_items
  else rendered_items
