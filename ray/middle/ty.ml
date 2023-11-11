open Printf
open Structures.Vec
open Def_id

type field = {
    def_id: def_id
  ; name: string
}

type variant = {
    def_id: def_id
  ; fields: field vec
}

type adt = {
    def_id: def_id
  ; variants: variant vec
}

let render_ty _ = "not implemented"

type int_ty =
  | I8
  | I16
  | I32
  | I64
  | Isize
  | U8
  | U16
  | U32
  | U64
  | Usize
(* [@@deriving enum] *)

let int_ty_to_enum = function
  | I8 -> 0
  | I16 -> 1
  | I32 -> 2
  | I64 -> 3
  | Isize -> 4
  | U8 -> 5
  | U16 -> 6
  | U32 -> 7
  | U64 -> 8
  | Usize -> 9
;;

let int_ty_of_enum = function
  | 0 -> Some I8
  | 1 -> Some I16
  | 2 -> Some I32
  | 3 -> Some I64
  | 4 -> Some Isize
  | 5 -> Some U8
  | 6 -> Some U16
  | 7 -> Some U32
  | 8 -> Some U64
  | 9 -> Some Usize
  | _ -> None
;;

let display_int_ty = function
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | Isize -> "isize"
  | U8 -> "u8"
  | U16 -> "u16"
  | U32 -> "u32"
  | U64 -> "u64"
  | Usize -> "usize"
;;

type float_ty =
  | F64
  | F32
(* [@@deriving enum] *)

let float_ty_to_enum = function F32 -> 0 | F64 -> 1
let float_ty_of_enum = function 0 -> Some F32 | 1 -> Some F64 | _ -> None
let display_float_ty = function F32 -> "f32" | F64 -> "f64"

type ty_vid = { index: int }

type infer_ty =
  | IntVar of ty_vid
  | FloatVar of ty_vid
  | TyVar of ty_vid

let i, f = ref 0, ref 0

let int_var_id () =
  let t = !i in
  incr i;
  t
;;

let float_var_id () =
  let t = !f in
  incr f;
  t
;;

let display_infer_ty = function
  | IntVar i -> sprintf "\x1b[1;31m?%di\x1b[0m" i.index
  | FloatVar i -> sprintf "\x1b[1;31m?%df\x1b[0m" i.index
  | TyVar i -> sprintf "\x1b[1;31m?%d\x1b[0m" i.index
;;

let render_infer_ty ty dbg =
  if dbg
  then display_infer_ty ty
  else
    match ty with
    | IntVar _ -> "int"
    | FloatVar _ -> "float"
    | TyVar _ -> "T"
;;

type ty =
  | Int of int_ty
  | Float of float_ty
  | Bool
  | Str
  | Ptr of ty
  | Ref of ty
  | Adt of def_id
  | FnPtr of def_id
  | Infer of infer_ty
  | Unit
  | Err
