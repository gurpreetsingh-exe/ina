open Ast
open Printf

let render (items : 'a list) (func : 'a -> string) (sep : string) : string =
  String.concat sep (List.map (fun item -> func item) items)

let render_ty (ty : ty) : string =
  match ty with
  | Prim ty -> (
    match ty with I32 -> "i32" | I64 -> "i64" | Bool -> "bool")

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
  match attr.kind, attr.style with
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
  let rendered_items =
    render modd.items (fun item -> render_item item) "\n"
  in
  if List.length modd.attrs > 0 then
    sprintf "%s\n\n%s"
      (render modd.attrs (fun attr -> render_attr attr) "\n")
      rendered_items
  else rendered_items
