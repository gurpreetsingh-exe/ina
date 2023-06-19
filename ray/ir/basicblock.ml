open Printf

type t = {
  mutable pred : t list;
  mutable succ : t list;
  mutable insts : Inst.t list;
  id : int;
}
[@@deriving show]

let _bb_id = ref 0

let bb_id () =
  let b = !_bb_id in
  incr _bb_id; b

let create () = { pred = []; succ = []; insts = []; id = bb_id () }

let render bb : string =
  sprintf "bb%s:\n" (string_of_int bb.id)
  ^ String.concat "\n" (List.map Inst.render_inst bb.insts)
