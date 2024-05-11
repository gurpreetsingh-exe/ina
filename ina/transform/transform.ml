module type Pass = Pass_manager.Pass

type passes = (module Pass) array

let passes : passes = [|(module Phi_elimination)|]

let run_passes tcx blocks =
  let passmgr = Pass_manager.create tcx passes blocks in
  Pass_manager.run_passes passmgr
;;
