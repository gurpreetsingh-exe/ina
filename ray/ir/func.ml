open Front
open Ast
open Printf

type fn_type = {
  name : string;
  linkage_name : string;
  args : (ty * string) list;
  ret_ty : ty;
  is_variadic : bool;
  is_extern : bool;
  abi : string;
}

let render_fn_type fn_ty =
  sprintf "fn %s(%s) -> %s" fn_ty.name
    (String.concat ","
       (List.map
          (fun (ty, name) -> sprintf "%s: %s" name (Fmt.render_ty ty))
          fn_ty.args)
    ^ if fn_ty.is_variadic then ", ..." else "")
    (Fmt.render_ty fn_ty.ret_ty)

type blocks = { mutable bbs : Inst.basic_block list }

type t =
  | Decl of fn_type
  | Def of {
      def_ty : fn_type;
      basic_blocks : blocks;
    }

let render = function
  | Decl fn_ty -> render_fn_type fn_ty ^ ";\n"
  | Def { def_ty; basic_blocks } ->
      sprintf "%s {\n%s\n}\n" (render_fn_type def_ty)
        (String.concat "\n\n" (List.map Basicblock.render basic_blocks.bbs))
