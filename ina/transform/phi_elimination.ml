open Ir
open Lowering

let name = "phi_elimination"
let is_enabled = ref true

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
        let ptr = bx#alloca ty (Source.Span.make 0 0) in
        branches#iter (function
            | Label bb, value ->
                let bx = new Builder.builder tcx blocks bb in
                bx#store value ptr (Source.Span.make 0 0)
            | _ -> assert false);
        phi.kind <- Move ptr
    | _ -> assert false
  in
  blocks.bbs#iter_if Basicblock.is_phi remove_phis
;;
