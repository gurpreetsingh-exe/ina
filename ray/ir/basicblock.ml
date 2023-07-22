open Printf

let create () : Inst.basic_block =
  Inst.{ pred = []; succ = []; insts = []; bid = -1; is_entry = false }

let label (bb : Inst.basic_block) : string = "bb" ^ string_of_int bb.bid

let append_pred (bb : Inst.basic_block) (succs : Inst.basic_block list) =
  bb.pred <- bb.pred @ succs

let append_succ (bb : Inst.basic_block) (succs : Inst.basic_block list) =
  bb.succ <- bb.succ @ succs;
  List.iter (fun b -> append_pred b [bb]) succs

let render (bb : Inst.basic_block) : string =
  let preds =
    if List.length bb.pred = 0 then ""
    else
      sprintf "                                              ; preds: %s"
        (String.concat ", " (List.map label bb.pred))
  in
  sprintf "%s:%s\n%s" (label bb) preds
    (String.concat "\n" (List.map Inst.render_inst bb.insts))
