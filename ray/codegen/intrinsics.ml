open Llvm

let gen_intrinsic name args builder ctx : llvalue =
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
