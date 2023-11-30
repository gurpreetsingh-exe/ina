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

let render tcx { ty; def_id; args; basic_blocks; _ } =
  let qpath = tcx#def_id_to_qpath#unsafe_get def_id in
  print_endline (print_def_id def_id);
  let ret = !ty |> function FnPtr { ret; _ } -> ret | _ -> assert false in
  sprintf
    "fn %s(%s)%s {\n%s\n}\n"
    (qpath#join "::" (fun s -> s))
    (args#join ", " (tcx |> Inst.render_value))
    (match ret with Unit -> String.empty | ty -> " -> " ^ render_ty ty)
    (basic_blocks.bbs#join "\n\n" (tcx |> Basicblock.render))
;;
