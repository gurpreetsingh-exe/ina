open Ir
open Lowering

let name = "phi_elimination"
let is_enabled _ = true

let run_pass (tcx : Middle.Ctx.tcx) (blocks : Func.blocks) =
  let remove_phis bb =
    let open Inst in
    let phi = bb.insts#get 0 in
    match phi.kind with
    | Phi (ty, branches) ->
        let bb =
          match branches#first with
          | Some (Label bb, _) when bb.pred#empty -> bb
          | Some (Label bb, _) -> bb.pred#get 0
          | _ -> assert false
        in
        let bx = new Builder.builder tcx blocks bb in
        let ptr = bx#alloca ty in
        branches#iter (function
            | Label bb, value ->
                let bx = new Builder.builder tcx blocks bb in
                bx#store value ptr
            | _ -> assert false);
        phi.kind <- Load ptr
    | _ -> assert false
  in
  blocks.bbs#iter_if Basicblock.is_phi remove_phis
;;
