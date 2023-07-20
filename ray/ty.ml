open Printf

type path_segment = string

type path = { mutable segments : path_segment list }

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

type ty_vid = { index : int }

type infer_ty =
  | IntVar of ty_vid
  | FloatVar of ty_vid
  | TyVar of ty_vid

let i, f = (ref 0, ref 0)

let int_var_id () =
  let t = !i in
  incr i; t

let float_var_id () =
  let t = !f in
  incr f; t

let display_infer_ty = function
  | IntVar i -> sprintf "\x1b[1;31m?%di\x1b[0m" i.index
  | FloatVar i -> sprintf "\x1b[1;31m?%df\x1b[0m" i.index
  | TyVar i -> sprintf "\x1b[1;31m?%d\x1b[0m" i.index

let render_infer_ty ty dbg =
  if dbg then display_infer_ty ty
  else (
    match ty with
    | IntVar _ -> "int"
    | FloatVar _ -> "float"
    | TyVar _ -> "T")

type ty =
  | Int of int_ty
  | Float of float_ty
  | Bool
  | Str
  | Ptr of ty
  | RefTy of ty
  | FnTy of ty list * ty * bool
  | Struct of string * (string * ty) list
  | Infer of infer_ty
  | Ident of path
  | Unit

let rec ( != ) (ty1 : ty) (ty2 : ty) : bool =
  match (ty1, ty2) with
  | Ident path, Struct (name, _) | Struct (name, _), Ident path ->
      String.concat "::" path.segments <> name
  | Ident path1, Ident path2 ->
      String.concat "::" path1.segments <> String.concat "::" path2.segments
  | Struct (name1, fields1), Struct (name2, fields2) ->
      let tys1 = List.map (fun (_, ty) -> ty) fields1 in
      let tys2 = List.map (fun (_, ty) -> ty) fields2 in
      (if List.length tys1 = 0 && List.length tys2 = 0 then false
      else (
        try
          List.map2 ( != ) tys1 tys2
          |> List.filter (fun f -> not f)
          |> List.length = 0
        with Invalid_argument _ -> true))
      || name1 <> name2
  | Ptr ty1, Ptr ty2 -> ty1 != ty2
  | RefTy ty1, RefTy ty2 -> ty1 != ty2
  | FnTy (tys1, ret_ty1, is_var1), FnTy (tys2, ret_ty2, is_var2) ->
      (if List.length tys1 = 0 && List.length tys2 = 0 then false
      else (
        try
          List.map2 ( != ) tys1 tys2
          |> List.filter (fun f -> not f)
          |> List.length = 0
        with Invalid_argument _ -> true))
      || ret_ty1 != ret_ty2 || is_var1 <> is_var2
  | _, _ -> ty1 <> ty2
