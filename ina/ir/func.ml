open Middle.Ty
open Structures.Vec
open Printf

type blocks = {
    locals: Inst.t vec
  ; bbs: Inst.basic_block vec
}

let render_blocks tcx { locals; bbs } =
  sprintf
    "{\nlocals:\n%s\n\n%s\n}"
    (locals#join "\n" (tcx |> Inst.render_inst))
    (bbs#join "\n\n" (tcx |> Basicblock.render))
;;

type t = {
    ty: ty ref
  ; instance: Inst.instance
  ; args: Inst.value vec
  ; basic_blocks: blocks
  ; decl: bool
}

let gen_id blocks =
  let open Inst in
  let bb_id = ref 0 in
  let inst_id = ref blocks.locals#len in
  let f inst =
    if Inst.has_value inst
    then (
      inst.id <- !inst_id;
      incr inst_id)
  in
  let f bb =
    bb.insts#iter f;
    bb.bid <- !bb_id;
    incr bb_id
  in
  blocks.bbs#iter f
;;

let render tcx { ty; instance; args; basic_blocks; _ } =
  let ret = Fn.ret tcx ty in
  sprintf
    "fn %s(%s)%s %s\n"
    (Inst.render_instance tcx instance)
    (args#join ", " (tcx |> Inst.render_value))
    (match !ret with Unit -> String.empty | _ -> " -> " ^ tcx#render_ty ret)
    (render_blocks tcx basic_blocks)
;;
