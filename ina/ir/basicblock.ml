open Printf
open Structures.Vec
open Inst
open Metadata

let create ?name () : basic_block =
  Inst.
    {
      pred = new vec
    ; succ = new vec
    ; insts = new vec
    ; terminator = RetUnit
    ; bid = -1
    ; name
    ; is_entry = false
    }
;;

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

let encode enc bb =
  Encoder.encode_vec enc bb.insts encode;
  encode_terminator enc bb.terminator;
  enc#emit_usize bb.bid
;;

let decode tcx dec =
  let bb = create () in
  Decoder.decode_vec dec bb.insts (decode tcx);
  bb.terminator <- decode_terminator tcx dec;
  bb.bid <- dec#read_usize;
  bb
;;

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
