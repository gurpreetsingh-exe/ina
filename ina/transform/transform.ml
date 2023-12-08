open Ir
open Lowering

module PhiElimination : Pass_manager.Pass = struct
  let name = "phi_elimination"
  let is_enabled _ = true

  let run_pass (tcx : Middle.Ctx.tcx) (blocks : Func.blocks) =
    let open Inst in
    let remove_phis bb =
      let phi = bb.insts#get 0 in
      match phi.kind with
      | Phi (_, pair) ->
          let pred =
            (List.map
               (function Label bb, _ -> bb.pred | _ -> assert false)
               pair
             |> List.hd)
              #get
              0
          in
          let bx = new Builder.builder tcx pred in
          let ptr = bx#alloca phi.ty in
          List.iter
            (function
             | Label bb, i ->
                 let bx = new Builder.builder tcx bb in
                 bx#store i ptr
             | _ -> assert false)
            pair;
          phi.kind <- Load ptr
      | _ -> assert false
    in
    blocks.bbs#iter_if Basicblock.is_phi remove_phis
  ;;
end

let run_passes tcx blocks =
  let passmgr = Pass_manager.create tcx [|(module PhiElimination)|] blocks in
  Pass_manager.run_passes passmgr
;;
