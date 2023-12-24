open Printf

type def_id = {
    inner: int
  ; unit_id: int
}

type def_kind =
  (* Type namespace *)
  | Mod
  | Struct
  (* Value namespace *)
  | Fn
  | Intrinsic
  | Impl
[@@deriving enum]

let def_id inner unit_id = { inner; unit_id }
let encode enc did = enc#emit_usize did.inner
let decode dec = def_id dec#read_usize dec#unit_id
let print_def_id def_id = sprintf "def_id#%d:%d" def_id.inner def_id.unit_id

let print_def_kind = function
  | Mod -> "module"
  | Struct -> "struct"
  | Fn -> "function"
  | Intrinsic -> "intrinsic"
  | Impl -> "impl"
;;
