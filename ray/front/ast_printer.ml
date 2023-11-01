open Ast
open Format

let out = ref String.empty
let ( += ) left right = left := !left ^ right

let green ?(bold = true) s =
  (if bold then "\x1b[1;32m" else "\x1b[32m") ^ s ^ "\x1b[0m"
;;

let red s = "\x1b[31m" ^ s ^ "\x1b[0m"
let blue s = "\x1b[1;34m" ^ s ^ "\x1b[0m"
let cyan s = "\x1b[1;36m" ^ s ^ "\x1b[0m"
let l () = out += "└─"
let t () = out += "├─"
let id prefix i = out += (green prefix ^ " " ^ red @@ sprintf "[0x%X]" i)
let q s = "'" ^ s ^ "'"

let render_children ?(prefix = "") ?(skip_last = false) items f =
  let nitems = items#len in
  let g i item =
    let last = i = nitems - 1 in
    out += prefix;
    out += if last && not skip_last then "└─" else "├─";
    f item (prefix ^ if last && not skip_last then "  " else "│ ")
  in
  items#iteri g
;;

let render_child ?(prefix = "") last item (f : 'a -> string -> unit) =
  out += prefix;
  out += if last then "└─" else "├─";
  f item (prefix ^ if last then "  " else "│ ")
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
  out += cyan fnsig.name;
  out += "\x1b[32m";
  out += " 'fn(";
  let nargs = fnsig.args#len in
  let f i ((ty, name) : ty * string) =
    let last = i = nargs - 1 in
    let arg =
      match ty.kind with
      | CVarArgs -> "..."
      | _ -> sprintf "%s: %s" name (render_ty ty)
    in
    out += (arg ^ if last then "" else ", ")
  in
  fnsig.args#iteri f;
  let ret_ty =
    match fnsig.ret_ty with Some ty -> " -> " ^ render_ty ty | None -> ""
  in
  out += sprintf ")%s'\x1b[0m" ret_ty

and render_stmt stmt prefix =
  match stmt with
  | Stmt expr | Expr expr -> render_expr expr prefix
  | Assert (expr, msg) ->
      out += green "Assert\n";
      let has_msg = Option.is_some msg in
      render_child ?prefix:(Some prefix) (not has_msg) expr render_expr;
      (match msg with
       | Some msg -> render_child ?prefix:(Some prefix) true msg render_expr
       | None -> ())
  | Assign (left, right) ->
      out += green "Assign\n";
      render_child ?prefix:(Some prefix) false left render_expr;
      render_child ?prefix:(Some prefix) true right render_expr
  | Binding { binding_pat; binding_ty; binding_expr; binding_id } ->
      id "Binding" binding_id;
      out += " ";
      (match binding_pat with PatIdent ident -> out += (cyan @@ q ident));
      (match binding_ty with
       | Some ty ->
           out += " ";
           out += (green ?bold:(Some false) @@ q @@ render_ty ty)
       | None -> ());
      out += "\n";
      render_child ?prefix:(Some prefix) true binding_expr render_expr

and render_path_segment (segment : path_segment) _ =
  render_ident segment.ident;
  out += "\n"

and render_ident ident = out += cyan ident

and render_binary_kind = function
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
  | Or -> "||"

and render_path_ path =
  cyan @@ q @@ path.segments#join "::" (fun s -> s.ident)

and render_expr expr prefix =
  match expr.expr_kind with
  | Lit lit ->
      out
      += green
           ?bold:(Some false)
           (match lit with
            | LitInt value -> sprintf "IntLit '%d'" value
            | LitFloat value -> sprintf "FloatLit '%f'" value
            | LitStr value -> sprintf "StrLit \"%s\"" (String.escaped value)
            | LitBool value -> sprintf "BoolLit '%b'" value);
      out += "\n"
  | Path path ->
      id "Path" path.path_id;
      out += " ";
      out += render_path_ path;
      out += "\n"
  | Call (expr, args) ->
      id "Call" expr.expr_id;
      let empty_args = args#empty in
      if empty_args then out += " no_args";
      out += "\n";
      render_child ?prefix:(Some prefix) empty_args expr render_expr;
      if not empty_args
      then
        render_children ?prefix:(Some prefix) args (fun f -> render_expr f)
  | If { cond; then_block; else_block; if_id; _ } ->
      let has_else = Option.is_some else_block in
      id "If" if_id;
      out += if has_else then " has_else" else " no_else";
      out += "\n";
      render_child ?prefix:(Some prefix) false cond render_expr;
      render_child
        ?prefix:(Some prefix)
        (not has_else)
        then_block
        render_block;
      if has_else
      then
        render_child
          ?prefix:(Some prefix)
          true
          (Option.get else_block)
          render_expr
  | Binary (kind, left, right) ->
      id "Binary" expr.expr_id;
      out += " ";
      out += (green ?bold:(Some false) @@ q @@ render_binary_kind kind);
      out += "\n";
      render_child ?prefix:(Some prefix) false left render_expr;
      render_child ?prefix:(Some prefix) true right render_expr
  | Block block -> render_block block prefix
  | Deref expr ->
      out += green "Deref\n";
      render_child ?prefix:(Some prefix) true expr render_expr
  | Ref expr ->
      out += green "Ref\n";
      render_child ?prefix:(Some prefix) true expr render_expr
  | StructExpr { struct_name; fields; struct_expr_id; _ } ->
      let has_fields = not fields#empty in
      id "StructExpr" struct_expr_id;
      out += " ";
      out += render_path_ struct_name;
      if not has_fields then out += " empty";
      out += "\n";
      let render_field (name, expr) prefix =
        out += cyan name;
        out += "\n";
        render_child ?prefix:(Some prefix) true expr render_expr
      in
      render_children ?prefix:(Some prefix) fields render_field
  | Field (expr, name) ->
      out += green "Field ";
      out += (cyan @@ q name);
      out += "\n";
      render_child ?prefix:(Some prefix) true expr render_expr
  | Cast (expr, ty) ->
      out += green "Cast ";
      out += (green ?bold:(Some false) @@ q @@ render_ty ty);
      out += "\n";
      render_child ?prefix:(Some prefix) true expr render_expr
  | MethodCall (expr, name, args) ->
      id "MethodCall" expr.expr_id;
      let empty_args = args#empty in
      out += (cyan @@ q name);
      if empty_args then out += " no_args";
      out += "\n";
      render_child ?prefix:(Some prefix) empty_args expr render_expr;
      if not empty_args
      then
        render_children ?prefix:(Some prefix) args (fun f -> render_expr f)

and render_block block prefix =
  id "Block" block.block_id;
  out += "\n";
  let has_last = Option.is_some block.last_expr in
  render_children
    block.block_stmts
    render_stmt
    ?skip_last:(Some has_last)
    ?prefix:(Some prefix);
  match block.last_expr with
  | Some expr -> render_child ?prefix:(Some prefix) true expr render_expr
  | None -> ()

and render_fn fn prefix =
  id "Fn" fn.func_id;
  out += " ";
  render_fn_sig fn.fn_sig;
  out += "\n";
  match fn.body with
  | Some body -> render_child ?prefix:(Some prefix) true body render_block
  | None -> ()

and render_struct strukt prefix =
  id "Struct" strukt.struct_id;
  out += (green ?bold:(Some false) @@ q strukt.ident ^ "\n");
  let render_field (ty, name) _ =
    out += (cyan @@ q name);
    out += ": ";
    out += (green ?bold:(Some false) @@ q @@ render_ty ty);
    out += "\n"
  in
  render_children ?prefix:(Some prefix) strukt.members render_field

and render_type ty = match ty with Struct strukt -> render_struct strukt

and render_impl impl prefix =
  id "Impl" impl.impl_id;
  out += " ";
  out += (green ?bold:(Some false) @@ q @@ render_ty impl.impl_ty);
  out += "\n";
  let render_item item = match item with AssocFn fn -> render_fn fn in
  render_children ?prefix:(Some prefix) impl.impl_items render_item

and render_item item prefix =
  match item with
  | Fn (fn, _) -> render_fn fn prefix
  | Unit name ->
      out += green "Unit ";
      out += (cyan @@ q name);
      out += "\n"
  | Type typ -> render_type typ prefix
  | Impl impl -> render_impl impl prefix
  | _ -> out += "not implemented\n"
;;

let render_module m =
  id "Mod" m.mod_id;
  out += "\n";
  render_children m.items render_item;
  out += "\n"
;;
