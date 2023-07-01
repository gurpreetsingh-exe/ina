open Printf

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

let signed_range (n : int) : int * int =
  let n = float_of_int (n - 1) in
  let lo = Float.pow (-2.0) n in
  let hi = Float.pow 2.0 n in
  (int_of_float lo, int_of_float hi - 1)

let unsigned_range (n : int) : int * int =
  let n = float_of_int n in
  let hi = Float.pow 2.0 n in
  (0, int_of_float hi - 1)

let integer_ranges = function
  | I8 -> signed_range 8
  | U8 -> unsigned_range 8
  | I16 -> signed_range 16
  | U16 -> unsigned_range 16
  | I32 -> signed_range 32
  | U32 -> unsigned_range 32
  | I64 | Isize -> signed_range 64
  | U64 | Usize -> unsigned_range 64

let is_unsigned = function
  | U8 | U16 | U32 | U64 | Usize -> true
  | _ -> false

let is_signed = function I8 | I16 | I32 | I64 | Isize -> true | _ -> false

type float_ty =
  | F64
  | F32

let display_float_ty = function F32 -> "f32" | F64 -> "f64"

type infer_ty =
  | IntVar of int
  | FloatVar of int
  | TyVar of int

let display_infer_ty = function
  | IntVar i -> sprintf "\x1b[1;31m?%di\x1b[0m" i
  | FloatVar i -> sprintf "\x1b[1;31m?%df\x1b[0m" i
  | TyVar i -> sprintf "\x1b[1;31m?%d\x1b[0m" i

type ty =
  | Int of int_ty
  | Float of float_ty
  | Bool
  | Str
  | Ptr of ty
  | RefTy of ty
  | FnTy of (ty list * ty * bool)
  | Infer of infer_ty
  | Unit
