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
  ; decl: bool
}

let gen_id blocks =
  let open Inst in
  let bb_id = ref 0 in
  let inst_id = ref 0 in
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

let render tcx { ty; def_id; args; basic_blocks; _ } =
  let qpath = tcx#def_id_to_qpath#unsafe_get def_id in
  let ret = !ty |> function FnPtr { ret; _ } -> ret | _ -> assert false in
  sprintf
    "fn %s(%s)%s {\n%s\n}\n"
    (qpath#join "::" (fun s -> s))
    (args#join ", " (tcx |> Inst.render_value))
    (match !ret with Unit -> String.empty | _ -> " -> " ^ render_ty ret)
    (basic_blocks.bbs#join "\n\n" (tcx |> Basicblock.render))
;;
