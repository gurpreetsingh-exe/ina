open Llvm

let gen_intrinsic name args builder ctx : llvalue =
  match name with
  | "as_ptr" ->
      let self = args.(0) in
      build_extractvalue self 0 "" builder
  | "len" ->
      let self = args.(0) in
      build_extractvalue self 1 "" builder
  | _ -> assert false
