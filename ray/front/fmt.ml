open Ast
open Printf

let render (items : 'a list) (func : 'a -> string) (sep : string) : string =
  String.concat sep (List.map (fun item -> func item) items)

let rec render_ty (ty : ty) : string =
  match ty with
  | Prim ty -> (
    match ty with I32 -> "i32" | I64 -> "i64" | Bool -> "bool")
  | Unit -> "()"
  | FnTy (ty_list, ret_ty) ->
      sprintf "fn(%s) -> %s"
        (render ty_list (fun ty -> render_ty ty) ", ")
        (render_ty ret_ty)

let render_fn_sig (fn_sig : fn_sig) : string =
  sprintf "fn %s(%s)%s" fn_sig.name
    (render fn_sig.args
       (fun (ty, name) ->
         sprintf "%s%s" (render_ty ty)
           (match name with Some name -> ": " ^ name | None -> ""))
       ", ")
    (match fn_sig.ret_ty with
    | Some ty -> " -> " ^ render_ty ty
    | None -> "")

let render_block (_ : block) : string = "{}"

let render_fn (func : func) : string =
  sprintf "%s %s"
    (render_fn_sig func.fn_sig)
    (match func.body with Some body -> render_block body | None -> ";")

let render_attr (attr : attr) : string =
  match (attr.kind, attr.style) with
  | NormalAttr attr, Outer -> sprintf "[%s]" attr.name
  | NormalAttr attr, Inner -> sprintf "![%s]" attr.name
  | Doc doc, _ -> doc

let render_const (constant : constant) : string = constant.const_name

let render_item (item : item) : string =
  match item with
  | Fn (func, attrs) ->
      sprintf "%s\n%s\n"
        (render attrs (fun attr -> render_attr attr) "\n")
        (render_fn func)
  | Const constant -> render_const constant
  | _ ->
      Printf.printf "todo\n";
      assert false

let render_mod modd : string =
  let rendered_items = render modd.items (fun item -> render_item item) "" in
  if List.length modd.attrs > 0 then
    sprintf "%s\n\n%s"
      (render modd.attrs (fun attr -> render_attr attr) "\n")
      rendered_items
  else rendered_items

(* debug utils *)

let display_lit (lit : lit) : string =
  match lit with
  | LitInt value -> sprintf "%d" value
  | LitBool value -> sprintf "%b" value

let display_expr_kind (expr_kind : expr_kind) =
  match expr_kind with Lit lit -> "Lit " ^ display_lit lit

let display_expr (expr : expr) =
  sprintf "{ id: %d, ty: %s, kind: %s }" expr.expr_id
    (match expr.expr_ty with Some ty -> render_ty ty | None -> "<none>")
    (display_expr_kind expr.expr_kind)

let display_stmt (stmt : stmt) indent =
  String.make indent ' '
  ^
  match stmt with
  | Stmt expr | Expr expr -> display_expr expr
  | Binding _ -> "binding"

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
