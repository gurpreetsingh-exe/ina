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

let render tcx { ty; def_id; args; basic_blocks } =
  let qpath = tcx#def_id_to_qpath#unsafe_get def_id in
  let ret = !ty |> function FnPtr { ret; _ } -> ret | _ -> assert false in
  sprintf
    "fn %s(%s)%s {\n%s\n}\n"
    (qpath#join "::" (fun s -> s))
    (args#join ", " Inst.render_value)
    (match ret with Unit -> String.empty | ty -> " -> " ^ render_ty ty)
    (basic_blocks.bbs#join "\n\n" Basicblock.render)
;;
