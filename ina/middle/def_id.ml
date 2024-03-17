open Printf

type def_id = {
    inner: int
  ; mod_id: int
}

type def_kind =
  (* Type namespace *)
  | Mod
  | Struct
  | TyParam
  (* Value namespace *)
  | Fn
  | AssocFn
  | Intrinsic
  | Impl
  (* TODO: can't move this up because of encoding bs *)
  | Adt
  | Cons
[@@deriving enum]

let def_id inner mod_id = { inner; mod_id }
let local_def_id inner = def_id inner 0

let encode enc did =
  enc#emit_usize did.inner;
  if did.mod_id = 0 then enc#emit_mod_id else enc#emit_usize did.mod_id
;;

let decode dec =
  let inner = dec#read_usize
  and mod_id = dec#read_usize in
  def_id inner mod_id
;;

let print_def_id def_id = sprintf "def_id#%d:%d" def_id.inner def_id.mod_id

let print_def_kind = function
  | Mod -> "module"
  | Struct -> "struct"
  | Adt -> "adt"
  | Cons -> "constructor"
  | TyParam -> "type parameter"
  | Fn -> "function"
  | AssocFn -> "associated function"
  | Intrinsic -> "intrinsic"
  | Impl -> "impl"
;;
