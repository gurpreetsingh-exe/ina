open Printf
open Structures.Vec
open Def_id

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

type tyvid = { index: int }
type intvid = { index: int }
type floatvid = { index: int }

type infer_ty =
  | IntVar of intvid
  | FloatVar of floatvid
  | TyVar of tyvid

let display_intvid (vid : intvid) = sprintf "\x1b[1;31m?%di\x1b[0m" vid.index
let display_floatvid vid = sprintf "\x1b[1;31m?%df\x1b[0m" vid.index

let display_infer_ty = function
  | IntVar i -> display_intvid i
  | FloatVar f -> display_floatvid f
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

type field =
  | Field of {
        ty: ty ref
      ; name: string
    }

and variant =
  | Variant of {
        def_id: def_id
      ; fields: field vec
    }

and adt = { variants: variant vec }

and abi =
  | Default
  | Intrinsic
  | C

and ty =
  | Int of int_ty
  | Float of float_ty
  | Bool
  | Str
  | Ptr of ty ref
  | Ref of ty ref
  | Adt of def_id
  | FnPtr of {
        args: ty ref vec
      ; ret: ty ref
      ; is_variadic: bool
      ; abi: abi
    }
  | Infer of infer_ty
  | Unit
  | Err

and t = ty

let rec hash = function
  | FnPtr { args; ret; is_variadic; abi } ->
      fold_left (fun init ty -> Hashtbl.hash (init + hash !ty)) 0 args
      + hash !ret
      + if is_variadic then 0 else 1 + Hashtbl.hash abi
  | Ptr ty -> 1 + hash !ty
  | Ref ty -> 2 + hash !ty
  | Adt { inner; _ } -> inner
  | ty -> Hashtbl.hash ty
;;

let equal t0 t1 = hash t0 = hash t1

let rec render_ty ty =
  match !ty with
  | Int i -> display_int_ty i
  | Float f -> display_float_ty f
  | Infer i ->
      (match i with
       | IntVar _ -> "{int}"
       | FloatVar _ -> "{float}"
       | TyVar _ -> "infer")
  | Unit -> "unit"
  | Bool -> "bool"
  | Str -> "str"
  | FnPtr { args; ret; is_variadic; abi } ->
      sprintf
        "%sfn(%s) -> %s"
        (abi |> function
         | Default -> ""
         | Intrinsic -> "\"intrinsic\" "
         | C -> "\"C\" ")
        (args#join ", " (fun ty -> render_ty ty)
         ^ if is_variadic then ", ..." else String.empty)
        (render_ty ret)
  | Ptr ty -> "*" ^ render_ty ty
  | Ref ty -> "&" ^ render_ty ty
  | Err -> "err"
  | Adt def_id -> print_def_id def_id
;;

let rec render_ty2 ty =
  match !ty with
  | Int i -> display_int_ty i
  | Float f -> display_float_ty f
  | Infer i -> display_infer_ty i
  | Unit -> "unit"
  | Bool -> "bool"
  | Str -> "str"
  | FnPtr { args; ret; is_variadic; abi } ->
      sprintf
        "%sfn(%s) -> %s"
        (abi |> function
         | Default -> ""
         | Intrinsic -> "\"intrinsic\" "
         | C -> "\"C\" ")
        (args#join ", " (fun ty -> render_ty2 ty)
         ^ if is_variadic then ", ..." else String.empty)
        (render_ty2 ret)
  | Err -> "err"
  | Ptr ty -> "*" ^ render_ty2 ty
  | Ref ty -> "&" ^ render_ty2 ty
  | Adt def_id -> print_def_id def_id
;;
(* | Adt { def_id; variants } -> *)
(*     let render (Field { name; ty }) = *)
(*       sprintf "%s: %s" name (render_ty2 ty) *)
(*     in *)
(*     let render (Variant { def_id; fields }) = *)
(*       sprintf "%s { %s }" (print_def_id def_id) (fields#join ", " render) *)
(*     in *)
(*     sprintf "%s { %s }" (print_def_id def_id) (variants#join ", " render) *)
