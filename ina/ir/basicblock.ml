open Printf
open Structures.Vec
open Inst

let create () : basic_block =
  Inst.
    {
      pred = new vec
    ; succ = new vec
    ; insts = new vec
    ; terminator = RetUnit
    ; bid = -1
    ; is_entry = false
    }
;;

let label bb : string = "bb" ^ string_of_int bb.bid

let is_phi bb =
  fold_left
    (fun hd rest -> hd || match rest.kind with Phi _ -> true | _ -> false)
    false
    bb.insts
;;

let append_pred bb pred = bb.pred#append pred

let append_succ bb succ =
  bb.succ#push succ;
  succ.pred#push bb
;;

let append_succs bb succs =
  bb.succ#append succs;
  succs#iter (fun bb0 -> bb0.pred#push bb)
;;

let encode enc bb = assert false
let decode dec = assert false

let render tcx bb : string =
  let preds =
    if bb.pred#empty
    then ""
    else
      sprintf
        "                                              ; preds: %s"
        (bb.pred#join ", " label)
  in
  sprintf
    "%s:%s\n%s%s"
    (label bb)
    preds
    (bb.insts#join "\n" (tcx |> Inst.render_inst))
    ((if bb.insts#empty then "" else "\n")
     ^ Inst.render_terminator tcx bb.terminator)
;;
