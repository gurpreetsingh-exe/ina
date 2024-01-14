open Printf
open Structures.Vec
open Def_id
open Metadata.Encoder
open Metadata.Decoder

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
[@@deriving enum]

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
[@@deriving enum]

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

type abi =
  | Default
  | Intrinsic
  | C
[@@deriving enum]

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

and typaram = {
    index: int
  ; name: string
}

and fnsig = {
    args: ty ref vec
  ; ret: ty ref
  ; is_variadic: bool
  ; abi: abi
}

and generic_arg = Ty of ty ref
and subst = Subst of generic_arg vec

and ty =
  | Int of int_ty
  | Float of float_ty
  | Bool
  | Str
  | Ptr of ty ref
  | Ref of ty ref
  | Adt of def_id
  | Fn of (def_id * subst)
  | FnPtr of fnsig
  | Param of typaram
  | Infer of infer_ty
  | Unit
  | Err

and t = ty

let discriminator = function
  | Int _ -> 1L
  | Float _ -> 2L
  | Bool -> 3L
  | Str -> 4L
  | Ptr _ -> 5L
  | Ref _ -> 6L
  | Adt _ -> 7L
  | FnPtr _ -> 8L
  | Infer _ -> 9L
  | Unit -> 10L
  | Param _ -> 11L
  | Fn _ -> 12L
  | Err -> 13L
;;

type fxhasher = { mutable hash: int }

let rotl64c x n = (x lsl n) lor (x lsr (-n land 62))
let rotate_left5 x = (x lsl 5) lor (x lsr 27)
let k = 0x145f306dc9c882ef
let fx_add_to_hash fx i = fx.hash <- rotl64c fx.hash 5 lxor i * k

module Fn = struct
  let render tcx { args; ret; is_variadic; abi } =
    sprintf
      "%sfn(%s) -> %s"
      (abi |> function
       | Default -> ""
       | Intrinsic -> "\"intrinsic\" "
       | C -> "\"C\" ")
      (args#join ", " (fun ty -> tcx#render_ty ty)
       ^ if is_variadic then ", ..." else String.empty)
      (tcx#render_ty ret)
  ;;

  let get tcx ty =
    match !ty with
    | Fn (did, subst) -> tcx#subst (tcx#get_fn did) subst
    | FnPtr fn -> fn
    | _ -> assert false
  ;;

  let args tcx ty = (get tcx ty).args
  let ret tcx ty = (get tcx ty).ret
  let abi tcx ty = (get tcx ty).abi
  let is_variadic tcx ty = (get tcx ty).is_variadic

  let is_generic ty =
    match !ty with
    | Fn (_, Subst subst) -> not subst#empty
    | FnPtr _ -> false
    | _ -> assert false
  ;;

  let subst ty =
    match ty with Fn (_, Subst subst) -> subst | _ -> assert false
  ;;

  let with_subst ty subst =
    match ty with Fn (did, _) -> Fn (did, Subst subst) | _ -> assert false
  ;;

  let __encode_fn : (encoder -> fnsig -> unit) ref =
    ref (fun _ _ -> assert false)
  ;;

  let encode enc fn = !__encode_fn enc fn
end

let _render_ty2 = ref (fun _ -> "")
let render_ty ty = !_render_ty2 ty

let rec encode enc ty =
  let disc = discriminator !ty in
  match !ty with
  | Int i -> enc#emit_with disc (fun e -> int_ty_to_enum i |> e#emit_usize)
  | Float f ->
      enc#emit_with disc (fun e -> float_ty_to_enum f |> e#emit_usize)
  | Bool | Str | Unit -> enc#emit_with disc (fun _ -> ())
  | Ptr ty | Ref ty -> enc#emit_with disc (fun e -> encode e ty)
  | Adt id -> enc#emit_with disc (fun e -> Def_id.encode e id)
  | Fn (id, _) -> enc#emit_with disc (fun e -> Def_id.encode e id)
  | FnPtr fn -> enc#emit_with disc (fun e -> Fn.encode e fn)
  | _ ->
      print_endline @@ render_ty ty;
      assert false
;;

Fn.__encode_fn :=
  fun enc { args; ret; is_variadic; abi } ->
    enc#emit_usize args#len;
    args#iter (fun ty -> encode enc ty);
    encode enc ret;
    enc#emit_bool is_variadic;
    abi_to_enum abi |> enc#emit_usize

let rec decode tcx dec =
  (match dec#read_usize with
   | 1 -> Int (dec#read_usize |> int_ty_of_enum |> Option.get)
   | 2 -> Float (dec#read_usize |> float_ty_of_enum |> Option.get)
   | 3 -> Bool
   | 4 -> Str
   | 5 -> Ptr (decode tcx dec)
   | 6 -> Ref (decode tcx dec)
   | 7 -> Adt (Def_id.decode dec)
   | 8 ->
       let args = new vec in
       let nargs = dec#read_usize in
       for _ = 0 to nargs - 1 do
         args#push (decode tcx dec)
       done;
       let ret = decode tcx dec in
       let is_variadic = dec#read_bool in
       let abi = dec#read_usize |> abi_of_enum |> Option.get in
       FnPtr { args; ret; is_variadic; abi }
   | 10 -> Unit
   | 12 -> Fn (Def_id.decode dec, Subst (new vec))
   | i ->
       printf "%d\n" i;
       assert false)
  |> tcx#intern
;;

let decode_fn tcx dec =
  let args = new vec in
  let nargs = dec#read_usize in
  for _ = 0 to nargs - 1 do
    args#push (decode tcx dec)
  done;
  let ret = decode tcx dec in
  let is_variadic = dec#read_bool in
  let abi = dec#read_usize |> abi_of_enum |> Option.get in
  { args; ret; is_variadic; abi }
;;

let encode_field enc (Field { ty; name }) =
  encode enc ty;
  enc#emit_str name
;;

let decode_field tcx dec =
  let ty = decode tcx dec in
  let name = dec#read_str in
  Field { ty; name }
;;

let encode_variant enc (Variant { def_id; fields }) =
  Def_id.encode enc def_id;
  encode_vec enc fields encode_field
;;

let decode_variant tcx dec =
  let def_id = Def_id.decode dec in
  let fields = new vec in
  decode_vec dec fields (tcx |> decode_field);
  Variant { def_id; fields }
;;

let rec hash hasher ty =
  let f = hash hasher in
  let g = fx_add_to_hash hasher in
  discriminator ty |> Int64.to_int |> g;
  match ty with
  | Int i -> g (int_ty_to_enum i + 1)
  | Float f -> g (float_ty_to_enum f + 1)
  | Bool | Str | Unit | Err -> ()
  | Ptr ty | Ref ty -> f !ty
  | Adt { inner; mod_id } ->
      g inner;
      g mod_id
  | Fn ({ inner; mod_id }, Subst subst) ->
      g inner;
      g mod_id;
      subst#iter (function Ty ty -> f !ty)
  | FnPtr { args; ret; is_variadic; abi } ->
      args#iter (fun ty -> f !ty);
      f !ret;
      if is_variadic then g 1;
      g (abi_to_enum abi + 1)
  | Infer infty ->
      (match infty with
       | IntVar i ->
           g 1;
           g i.index
       | FloatVar f ->
           g 2;
           g f.index
       | TyVar t ->
           g 3;
           g t.index)
  | Param { name; index } ->
      g (String.hash name);
      g index
;;

let fnhash { args; ret; is_variadic; abi } =
  let hasher = { hash = Int.zero } in
  let f = hash hasher in
  let g = fx_add_to_hash hasher in
  args#iter (fun ty -> f !ty);
  f !ret;
  if is_variadic then g 1;
  g (abi_to_enum abi + 1)
;;

let hash ty =
  let hasher = { hash = Int.zero } in
  hash hasher ty;
  hasher.hash
;;

let equal t0 t1 = hash t0 = hash t1

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
  | Adt def_id -> sprintf "adt(%s)" (print_def_id def_id)
  | Fn (def_id, _) -> sprintf "fn(%s)" (print_def_id def_id)
  | Param { index; name } -> sprintf "%s%d" name index
;;

_render_ty2 := render_ty2
