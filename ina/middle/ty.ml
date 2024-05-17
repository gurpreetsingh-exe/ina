open Printf
open Structures.Vec
open Def_id
open Metadata.Encoder
open Metadata.Decoder
open Structures.Hashmap

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

let display_floatvid (vid : floatvid) =
  sprintf "\x1b[1;31m?%df\x1b[0m" vid.index
;;

let display_tyvid (vid : tyvid) = sprintf "\x1b[1;31m?%d\x1b[0m" vid.index

let display_infer_ty = function
  | IntVar i -> display_intvid i
  | FloatVar f -> display_floatvid f
  | TyVar i -> display_tyvid i
;;

let render_infer_ty ty dbg =
  if dbg
  then display_infer_ty ty
  else
    match ty with
    | IntVar _ -> "integer"
    | FloatVar _ -> "float"
    | TyVar _ -> "_"
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
      ; index: int
    }

and adtkind =
  | StructT
  | AdtT

and adt = {
    variants: variant vec
  ; kind: adtkind
}

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

and mutability =
  | Mut
  | Imm

and ty =
  | Int of int_ty
  | Float of float_ty
  | Bool
  | Str
  | Ptr of (mutability * ty ref)
  | Ref of (mutability * ty ref)
  | Adt of (def_id * subst)
  | Tuple of ty ref vec
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
  | Tuple _ -> 14L
;;

type fxhasher = { mutable hash: int }

let rotl64c x n = (x lsl n) lor (x lsr (-n land 62))
let rotate_left5 x = (x lsl 5) lor (x lsr 27)
let k = 0x145f306dc9c882ef
let fx_add_to_hash fx i = fx.hash <- rotl64c fx.hash 5 lxor i * k

module Generics = struct
  type generic_param_def = {
      name: string
    ; def_id: def_id
    ; index: int
  }

  type t = {
      parent: def_id option
    ; parent_count: int
    ; params: generic_param_def vec
  }

  let empty = { parent = None; params = new vec; parent_count = 0 }
  let count self = self.parent_count + self.params#len

  let rec to_subst self tcx =
    let parent_subst =
      match self.parent with
      | Some did -> to_subst (tcx#generics_of did) tcx
      | None -> new vec
    in
    let child_subst =
      map self.params (fun { name; index; _ } ->
          Ty (tcx#ty_param index name))
    in
    parent_subst#append child_subst;
    parent_subst
  ;;

  let to_subst_parent self tcx =
    match self.parent with
    | Some did ->
        let parent = tcx#generics_of did in
        to_subst parent tcx
    | None -> new vec
  ;;

  let to_subst_child self tcx =
    map self.params (fun { name; index; _ } ->
        Ty (tcx#ty_param (index - self.parent_count) name))
  ;;

  let encode enc generics =
    encode_option enc generics.parent Def_id.encode;
    enc#emit_usize generics.parent_count;
    encode_vec enc generics.params (fun e param ->
        e#emit_str param.name;
        Def_id.encode e param.def_id;
        e#emit_usize param.index)
  ;;

  let decode dec =
    let parent = decode_option dec Def_id.decode in
    let parent_count = dec#read_usize in
    let params = new vec in
    decode_vec dec params (fun d ->
        let name = d#read_str in
        let def_id = Def_id.decode d in
        let index = d#read_usize in
        { name; def_id; index });
    { parent; parent_count; params }
  ;;

  let display_param param =
    sprintf
      "{ name = %s; def_id = %s; index = %d }"
      param.name
      (print_def_id param.def_id)
      param.index
  ;;

  let display self =
    sprintf
      "{ parent = %s; parent_count = %d; params = [%s] }"
      (match self.parent with
       | Some parent -> print_def_id parent
       | None -> "None")
      self.parent_count
      (self.params#join ", " display_param)
  ;;
end

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
    match !ty with Fn (_, Subst subst) -> subst | _ -> assert false
  ;;

  let with_subst tcx ty subst =
    match !ty with
    | Fn (did, _) -> tcx#intern (Fn (did, subst))
    | _ -> assert false
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
  | Ptr (m, ty) | Ref (m, ty) -> enc#emit_with disc (fun e -> encode e ty)
  | Adt (id, subst) | Fn (id, subst) ->
      enc#emit_with disc (fun e ->
          Def_id.encode e id;
          encode_subst e subst)
  | FnPtr fn -> enc#emit_with disc (fun e -> Fn.encode e fn)
  | Param { name; index } ->
      enc#emit_with disc (fun e ->
          e#emit_str name;
          e#emit_usize index)
  | _ ->
      print_endline @@ render_ty ty;
      assert false

and encode_subst enc (Subst subst) =
  encode_vec enc subst (fun e (Ty ty) -> encode e ty)
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
   | 5 -> Ptr (Imm, decode tcx dec)
   | 6 -> Ref (Imm, decode tcx dec)
   | 7 ->
       let did = Def_id.decode dec in
       let subst = decode_subst tcx dec in
       Adt (did, subst)
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
   | 11 ->
       let name = dec#read_str
       and index = dec#read_usize in
       Param { name; index }
   | 12 ->
       let did = Def_id.decode dec in
       let subst = decode_subst tcx dec in
       Fn (did, subst)
   | i ->
       printf "%d\n" i;
       assert false)
  |> tcx#intern

and decode_subst tcx dec =
  let subst = new vec in
  decode_vec dec subst (fun d -> Ty (decode tcx d));
  Subst subst
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

let encode_variant enc (Variant { def_id; fields; index }) =
  Def_id.encode enc def_id;
  encode_vec enc fields encode_field;
  enc#emit_usize index
;;

let decode_variant tcx dec =
  let def_id = Def_id.decode dec in
  let fields = new vec in
  decode_vec dec fields (tcx |> decode_field);
  let index = dec#read_usize in
  tcx#variant def_id fields index
;;

let rec hash hasher ty =
  let f = hash hasher in
  let g = fx_add_to_hash hasher in
  discriminator ty |> Int64.to_int |> g;
  match ty with
  | Int i -> g (int_ty_to_enum i + 1)
  | Float f -> g (float_ty_to_enum f + 1)
  | Bool | Str | Unit | Err -> ()
  | Ptr (m, ty) | Ref (m, ty) ->
      g (m |> function Mut -> 1 | Imm -> 2);
      f !ty
  | Adt ({ inner; mod_id }, Subst subst) ->
      g inner;
      g mod_id;
      subst#iter (function Ty ty -> f !ty)
  | Tuple tys -> tys#iter (fun ty -> f !ty)
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
  g (abi_to_enum abi + 1);
  hasher.hash
;;

let hash ty =
  let hasher = { hash = Int.zero } in
  hash hasher ty;
  hasher.hash
;;

let equal t0 t1 = hash t0 = hash t1
let mut = function Mut -> "mut " | Imm -> ""

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
  | Ptr (m, ty) -> "*" ^ mut m ^ render_ty2 ty
  | Ref (m, ty) -> "&" ^ mut m ^ render_ty2 ty
  | Adt (def_id, _) -> sprintf "adt(%s)" (print_def_id def_id)
  | Tuple tys -> tys#join ", " render_ty2
  | Fn (def_id, _) -> sprintf "fn(%s)" (print_def_id def_id)
  | Param { index; name } -> sprintf "%s%d" name index
;;

_render_ty2 := render_ty2
