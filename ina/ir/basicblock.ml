open Printf
open Structures.Vec

let create () : Inst.basic_block =
  Inst.
    {
      pred = new vec
    ; succ = new vec
    ; insts = new vec
    ; bid = -1
    ; is_entry = false
    }
;;

let label (bb : Inst.basic_block) : string = "bb" ^ string_of_int bb.bid
let append_pred (bb : Inst.basic_block) pred = bb.pred#append pred

let append_succ (bb : Inst.basic_block) succ =
  bb.succ#push succ;
  succ.pred#push bb
;;

let append_succs (bb : Inst.basic_block) succs =
  bb.succ#append succs;
  succs#iter (fun bb0 -> bb0.pred#push bb)
;;

let render tcx (bb : Inst.basic_block) : string =
  let preds =
    if bb.pred#empty
    then ""
    else
      sprintf
        "                                              ; preds: %s"
        (bb.pred#join ", " label)
  in
  sprintf
    "%s:%s\n%s"
    (label bb)
    preds
    (bb.insts#join "\n" (tcx |> Inst.render_inst))
;;
