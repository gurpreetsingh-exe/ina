open Middle.Ty
open Middle.Def_id
open Structures.Vec
open Printf

type blocks = { bbs: Inst.basic_block vec }

type t = {
    ty: ty ref
  ; def_id: def_id
  ; args: Inst.value vec
  ; basic_blocks: blocks
}

let render { ty; def_id; args; basic_blocks } =
  sprintf
    "%s(%s): %s {\n%s\n}\n"
    (print_def_id def_id)
    (args#join ", " Inst.render_value)
    (render_ty !ty)
    (basic_blocks.bbs#join "\n\n" Basicblock.render)
;;
