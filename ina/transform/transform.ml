type passes = (module Pass_manager.Pass) array

let passes : passes = [|(module Phi_elimination)|]

let run_passes tcx blocks =
  let passmgr = Pass_manager.create tcx passes blocks in
  Pass_manager.run_passes passmgr
;;
