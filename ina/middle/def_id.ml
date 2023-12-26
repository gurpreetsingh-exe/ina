open Printf

type def_id = {
    inner: int
  ; extmod_id: int
}

type def_kind =
  (* Type namespace *)
  | Mod
  | Struct
  | TyParam
  (* Value namespace *)
  | Fn
  | Intrinsic
  | Impl
[@@deriving enum]

let def_id inner extmod_id = { inner; extmod_id }
let local_def_id inner = def_id inner 0
let encode enc did = enc#emit_usize did.inner
let decode dec = def_id dec#read_usize dec#extmod_id

let print_def_id def_id =
  sprintf "def_id#%d:%d" def_id.inner def_id.extmod_id
;;

let print_def_kind = function
  | Mod -> "module"
  | Struct -> "struct"
  | TyParam -> "type parameter"
  | Fn -> "function"
  | Intrinsic -> "intrinsic"
  | Impl -> "impl"
;;
