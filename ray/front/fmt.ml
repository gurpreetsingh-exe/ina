open Ast
open Format
open Structures.Vec

let out = ref String.empty
let indent = ref @@ -1
let ( += ) left right = left := !left ^ right
let reset () = out += "\x1b[0m"

let stack =
  let v = new vec in
  v#resize 100 false;
  v
;;

(* let green () = out += "\x1b[1;32m" *)
let green s = "\x1b[1;32m" ^ s ^ "\x1b[0m"
let space () = String.make (4 * !indent) ' '
let brace () = String.make (4 * !indent) ' ' ^ "}"

let render_children items f =
  incr indent;
  let nitems = items#len in
  let f i item =
    let last = i = nitems - 1 in
    let prefix = if last then "└─" else "├─" in
    if !indent - 1 = 0 then out += " │" else out += " ";
    out += space ();
    out += prefix;
    f item
  in
  items#iteri f;
  decr indent
;;

let display_int_ty = function
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
;;

let display_float_ty = function F32 -> "f32" | F64 -> "f64"
let render_path path = path.segments#join "::" (fun seg -> seg.ident)

let rec render_ty (ty : ty) =
  match ty.kind with
  | Int ty -> display_int_ty ty
  | Float ty -> display_float_ty ty
  | Bool -> "bool"
  | Str -> "str"
  | Ptr ty -> sprintf "*%s" (render_ty ty)
  | Ref ty -> sprintf "&%s" (render_ty ty)
  | Unit -> "()"
  | FnTy (ty_list, ret_ty, _) ->
      sprintf
        "fn(%s) -> %s"
        (ty_list#join ", " (fun ty -> render_ty ty))
        (render_ty ret_ty)
  | Path path -> render_path path
  | ImplicitSelf -> "self"
  | CVarArgs -> "..."
  | Err -> "error"

and render_fn_sig fnsig =
  out += sprintf "%s(" fnsig.name;
  let nargs = fnsig.args#len in
  let f i ((ty, name) : ty * string) =
    let last = i = nargs - 1 in
    let arg =
      match ty.kind with
      | CVarArgs -> "..."
      | _ -> sprintf "%s: %s" name (green @@ render_ty ty)
    in
    out += (arg ^ if last then "" else ", ")
  in
  fnsig.args#iteri f;
  let ret_ty =
    match fnsig.ret_ty with
    | Some ty -> " -> " ^ green @@ render_ty ty
    | None -> ""
  in
  out += sprintf ")%s" ret_ty

and render_stmt stmt =
  let s =
    match stmt with
    | Stmt _ -> "Stmt"
    | Expr _ -> "Expr"
    | Assert _ -> "Assert"
    | Assign _ -> "Assign"
    | Binding _ -> "Binding"
  in
  out += s;
  out += "\n"

and render_block block = render_children block.block_stmts render_stmt

and render_fn fn =
  render_fn_sig fn.fn_sig;
  out += "\n";
  match fn.body with Some body -> render_block body | None -> ()

and render_struct strukt =
  out += "Struct ";
  out += (strukt.ident ^ "\n");
  let render_field (ty, name) =
    out += sprintf "%s: %s\n" name (green @@ render_ty ty)
  in
  render_children strukt.members render_field

and render_type ty = match ty with Struct strukt -> render_struct strukt

and render_impl impl =
  out += "Impl ";
  out += (green @@ render_ty impl.impl_ty);
  out += "\n";
  let render_item item = match item with AssocFn fn -> render_fn fn in
  render_children impl.impl_items render_item

and render_item item =
  match item with
  | Fn (fn, _) ->
      out += "Fn ";
      render_fn fn
  | Unit name ->
      out += "Unit ";
      out += (name ^ "\n")
  | Type typ -> render_type typ
  | Impl impl -> render_impl impl
  | _ -> out += "not implemented\n"
;;

let render_module m =
  out += "Mod\n";
  render_children m.items render_item
;;

let render_mod m =
  let render_children items f =
    incr indent;
    let f _ item =
      out += space ();
      f item
    in
    items#iteri f;
    decr indent
  in
  let rec render_fn_sig fnsig =
    out += sprintf "fn %s(" fnsig.name;
    let nargs = fnsig.args#len in
    let f i ((ty, name) : ty * string) =
      let last = i = nargs - 1 in
      let arg =
        match ty.kind with
        | CVarArgs -> "..."
        | _ -> sprintf "%s: %s" name (green @@ render_ty ty)
      in
      out += (arg ^ if last then "" else ", ")
    in
    fnsig.args#iteri f;
    let ret_ty =
      match fnsig.ret_ty with
      | Some ty -> " -> " ^ green @@ render_ty ty
      | None -> ""
    in
    out += sprintf ")%s" ret_ty
  and render_expr expr =
    match expr.expr_kind with
    | Lit lit ->
        out
        +=
        (match lit with
         | LitInt v -> string_of_int v
         | LitFloat v -> string_of_float v
         | LitBool v -> string_of_bool v
         | LitStr v -> "\"" ^ String.escaped v ^ "\"")
    | Path path -> out += render_path path
    | Call (expr, exprs) ->
        render_expr expr;
        out += "(";
        let l = exprs#len in
        exprs#iteri (fun i expr ->
            let last = i = l - 1 in
            render_expr expr;
            if not last then out += ", ");
        out += ")"
    | Binary (_, left, right) ->
        render_expr left;
        out += " + ";
        render_expr right
    | If { cond; then_block; else_block; _ } ->
        out += "if ";
        render_expr cond;
        out += " ";
        render_block then_block;
        (match else_block with
         | Some elze ->
             out += " else ";
             render_expr elze
         | None -> ())
    | Block block -> render_block block
    | Deref expr ->
        out += "*";
        render_expr expr
    | Ref expr ->
        out += "&";
        render_expr expr
    | StructExpr { struct_name; fields; _ } ->
        out += render_path struct_name;
        out += " { ";
        let l = fields#len in
        fields#iteri (fun i (name, expr) ->
            let last = i = l - 1 in
            out += name;
            out += ": ";
            render_expr expr;
            if not last then out += ", ")
    | Field (expr, name) ->
        render_expr expr;
        out += ("." ^ name)
    | Cast (expr, ty) ->
        render_expr expr;
        out += (" as " ^ render_ty ty)
    | MethodCall (expr, name, exprs) ->
        render_expr expr;
        out += ("." ^ name ^ "(");
        let l = exprs#len in
        exprs#iteri (fun i expr ->
            let last = i = l - 1 in
            render_expr expr;
            if not last then out += ", ");
        out += ")"
  and render_stmt stmt =
    let s () =
      match stmt with
      | Stmt expr ->
          render_expr expr;
          out += ";"
      | Expr expr -> render_expr expr
      | Assert (expr, msg) ->
          out += "assert ";
          render_expr expr;
          (match msg with
           | Some expr ->
               out += ", ";
               render_expr expr
           | None -> ())
      | Assign (left, right) ->
          render_expr left;
          out += " = ";
          render_expr right
      | Binding binding ->
          out += "let ";
          (binding.binding_pat |> function PatIdent name -> out += name);
          (match binding.binding_ty with
           | Some ty ->
               out += ": ";
               out += render_ty ty
           | None -> ());
          out += " = ";
          render_expr binding.binding_expr;
          out += ";"
    in
    s ();
    out += "\n"
  and render_block block =
    out += "{\n";
    render_children block.block_stmts render_stmt;
    (match block.last_expr with
     | Some expr ->
         render_children
           (let v = new vec in
            v#push expr;
            v)
           render_expr;
         out += "\n"
     | None -> ());
    out += brace ()
  and render_fn fn =
    render_fn_sig fn.fn_sig;
    out += " ";
    match fn.body with
    | Some body ->
        render_block body;
        out += "\n"
    | None -> ()
  and render_struct strukt =
    out += (strukt.ident ^ " = {\n");
    let render_field (ty, name) =
      out += sprintf "%s: %s,\n" name (green @@ render_ty ty)
    in
    render_children strukt.members render_field;
    out += brace ();
    out += "\n"
  and render_type ty = match ty with Struct strukt -> render_struct strukt
  and render_impl impl =
    out += "impl ";
    out += (green @@ render_ty impl.impl_ty);
    out += " {\n";
    let render_item item = match item with AssocFn fn -> render_fn fn in
    render_children impl.impl_items render_item;
    out += brace ();
    out += "\n"
  and render_item item =
    match item with
    | Fn (fn, _) -> render_fn fn
    | Unit name -> out += ("unit " ^ name ^ ";\n")
    | Type typ -> render_type typ
    | Impl impl -> render_impl impl
    | _ -> out += "not implemented\n"
  in
  render_children m.items render_item
;;
