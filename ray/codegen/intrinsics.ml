open Llvm
open Ty

let gen_intrinsic name args builder tcx : llvalue =
  let ctx = tcx.out_mod.llcx in
  match name with
  | "as_ptr" ->
      let self = args.(0) in
      build_extractvalue self 0 "" builder
  | "len" ->
      let self = args.(0) in
      build_extractvalue self 1 "" builder
  | "offset" ->
      let self = args.(0) in
      let off = args.(1) in
      build_gep (i8_type ctx) self [|off|] "" builder
  | _ -> assert false
