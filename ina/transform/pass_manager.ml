open Middle.Ctx
open Ir
open Structures.Vec

module type Pass = sig
  val name : string
  val is_enabled : tcx -> bool
  val run_pass : tcx -> Func.blocks -> unit
end

type t = {
    tcx: tcx
  ; passes: (module Pass) vec
  ; blocks: Func.blocks
}

let create tcx passes blocks =
  let passmgr = { tcx; passes = new vec; blocks } in
  passmgr.passes#replace passes;
  passmgr
;;

let run_passes passmgr =
  let tcx = passmgr.tcx in
  passmgr.passes#iter (fun (module P : Pass) ->
      if P.is_enabled tcx then P.run_pass tcx passmgr.blocks);
  Func.gen_id passmgr.blocks
;;

let add_pass passmgr pass = passmgr.passes#push pass
