open Printf

let create () : Inst.basic_block =
  Inst.{ pred = []; succ = []; insts = []; bid = -1 }

let render (bb : Inst.basic_block) : string =
  sprintf "bb%s:\n%s" (string_of_int bb.bid)
    (String.concat "\n" (List.map Inst.render_inst bb.insts))
