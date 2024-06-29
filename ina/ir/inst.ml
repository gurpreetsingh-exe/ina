open Middle
open Middle.Ty
open Middle.Def_id
open Printf
open Structures.Vec
open Source

type instance_def =
  | Fn of def_id
  | Intrinsic of def_id
  | Test of def_id

and instance = {
    def: instance_def
  ; subst: subst
}

and item = Fn of instance

let encode_instance enc { def; subst } =
  (match def with
   | Fn did -> enc#emit_with 0L (fun e -> Def_id.encode e did)
   | Intrinsic did -> enc#emit_with 1L (fun e -> Def_id.encode e did)
   | Test did -> enc#emit_with 2L (fun e -> Def_id.encode e did));
  Ty.encode_subst enc subst
;;

module Instance = struct
  type t = instance

  let hash hasher { def; subst = Subst subst } =
    let g = fx_add_to_hash hasher in
    let { inner; mod_id } =
      match def with Fn did | Intrinsic did | Test did -> did
    in
    g inner;
    g mod_id;
    subst#iter (function Ty ty -> g @@ Ty.hash !ty)
  ;;

  let hash i =
    let hasher = { hash = Int.zero } in
    hash hasher i;
    hasher.hash
  ;;

  let equal a b = hash a = hash b
end

module InstanceMap = Hashtbl.Make (Instance)

let decode_instance tcx dec =
  let def =
    match dec#read_usize with
    | 0 -> Fn (Def_id.decode dec)
    | 1 -> Intrinsic (Def_id.decode dec)
    | 2 -> Test (Def_id.decode dec)
    | _ -> assert false
  in
  let subst = Ty.decode_subst tcx dec in
  { def; subst }
;;

let instance_def_id instance =
  instance.def |> function Fn id | Intrinsic id | Test id -> id
;;

let render_instance (tcx : Middle.Ctx.tcx) instance =
  match instance.def with
  | Fn id | Intrinsic id | Test id ->
      let (Subst subst) = instance.subst in
      sprintf
        "%s@%s%s"
        (match instance.def with
         | Intrinsic _ -> "i"
         | Test _ -> "t"
         | _ -> "")
        (tcx#into_segments id
         |> function segments, _ -> segments |> String.concat "::")
        (if subst#empty
         then ""
         else
           sprintf
             "[%s]"
             (subst#join ", " (function Ty ty -> tcx#render_ty ty)))
;;

type binary_kind =
  | Add
  | Sub
  | Mul
  | Div
  | Gt
  | Lt
  | Eq
  | NotEq
  | GtEq
  | LtEq
  | BitAnd
  | BitOr
  | And
  | Or
[@@deriving enum]

type t = {
    mutable kind: inst_kind
  ; ty: ty ref
  ; mutable id: int
  ; span: Span.t
}

and coercion = ArrayToSlice

and inst_kind =
  | Alloca of ty ref
  | Binary of binary_kind * value * value
  (* (bb * inst) *)
  | Phi of ty ref * (value * value) vec
  | Aggregate of (aggregate * value vec)
  | Discriminant of value
  | Payload of (value * int)
  | Store of value * value
  | Copy of value
  | Move of value
  | Gep of ty ref * value * int
  | Index of ty ref * value * value
  | Len of value
  | Call of ty ref * value * value vec
  | Intrinsic of string * value vec
  | Trap of string
  | BitCast of (value * ty ref)
  | Zext of (value * ty ref)
  | Trunc of (value * ty ref)
  (* Sext of ty * value *)
  (* Fpext of ty * value *)
  (* Fptrunc of ty * value *)
  | PtrToInt of (value * ty ref)
  | IntToPtr of (value * ty ref)
  | Coercion of (coercion * value * ty ref)
  | Nop

and terminator =
  | Switch of (value * (value * value) vec * value option)
  | Br of value * value * value
  | Jmp of value
  | Ret of value
  | RetUnit

and aggregate =
  | Adt of (def_id * int * subst)
  | Slice of ty ref
  | Array of ty ref
  | Repeat of (ty ref * int)

and value =
  | Const of {
        kind: const_kind
      ; ty: ty ref
    }
  | VReg of t
  | Label of basic_block
  | Param of ty ref * string * int
  | Global of item

and const_kind =
  | Int of int
  | Float of float
  | Str of string
  | Bool of bool
  | Struct of value vec
  | Unit

and basic_block = {
    mutable pred: basic_block vec
  ; mutable succ: basic_block vec
  ; mutable insts: t vec
  ; mutable terminator: terminator
  ; mutable bid: int
  ; mutable name: string option
  ; mutable is_entry: bool
}
(* [@@deriving show] *)

let inst_kind_to_enum = function
  | Alloca _ -> 0L
  | Binary _ -> 1L
  | Phi _ -> 2L
  | Aggregate _ -> 3L
  | Discriminant _ -> 4L
  | Payload _ -> 5L
  | Store _ -> 6L
  | Copy _ -> 7L
  | Move _ -> 8L
  | Gep _ -> 9L
  | Call _ -> 10L
  | Intrinsic _ -> 11L
  | Trap _ -> 12L
  | BitCast _ -> 13L
  | Zext _ -> 14L
  | Trunc _ -> 15L
  | PtrToInt _ -> 16L
  | IntToPtr _ -> 17L
  | Nop -> 18L
  | Index _ -> 19L
  | Len _ -> 20L
  | Coercion _ -> 21L
;;

let binary_kind_to_inst = function
  | Ast.Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Gt -> Gt
  | Lt -> Lt
  | Eq -> Eq
  | NotEq -> NotEq
  | GtEq -> GtEq
  | LtEq -> LtEq
  | BitAnd -> BitAnd
  | And -> And
  | BitOr -> BitOr
  | Or -> Or
;;

let render_binary = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Gt -> "gt"
  | Lt -> "lt"
  | Eq -> "eq"
  | NotEq -> "neq"
  | GtEq -> "gte"
  | LtEq -> "lte"
  | BitAnd | And -> "and"
  | BitOr | Or -> "or"
;;

let has_value inst = !(inst.ty) <> Unit

let label bb : string =
  let s = string_of_int bb.bid in
  Option.value bb.name ~default:"bb" ^ s
;;

let rec render_const tcx = function
  | Int value -> sprintf "%d" value
  | Float value -> sprintf "%f" value
  | Str value -> sprintf "\"%s\"" (String.escaped value)
  | Bool value -> sprintf "%b" value
  | Struct values ->
      sprintf "{ %s }" (values#join ", " (tcx |> render_value))
  | Unit -> "()"

and render_value tcx = function
  | Const const ->
      sprintf "%s %s" (tcx#render_ty const.ty) (render_const tcx const.kind)
  | VReg inst -> sprintf "%s %%%i" (tcx#render_ty inst.ty) inst.id
  | Label bb ->
      sprintf "label %%%s" (label bb) (* (sprintf "label %%bb%d" bb.bid) *)
  | Param (ty, name, _) -> sprintf "%s %%%s" (tcx#render_ty ty) name
  | Global (Fn instance) -> render_instance tcx instance
;;

let get_ty tcx = function
  | Param (ty, _, _) -> ty
  | Const c -> c.ty
  | VReg v -> v.ty
  | Global (Fn { def; subst = Subst subst }) ->
      Middle.Ctx.SubstFolder.fold_ty
        tcx
        (match def with Fn id | Intrinsic id | Test id -> tcx#get_def id)
        subst
  | _ -> assert false
;;

let extract_block = function Label bb -> bb | _ -> assert false

let render_terminator tcx term =
  "    "
  ^
  match term with
  | Switch (cond, args, default) ->
      sprintf
        "switch %s, [%s%s]"
        (render_value tcx cond)
        (args#join ", " (fun (bb, inst) ->
             sprintf "%s: %s" (render_value tcx bb) (render_value tcx inst)))
        (default
         |> Option.map (render_value tcx)
         |> Option.map (sprintf ", default: %s")
         |> Option.value ~default:"")
  | Br (cond, true_block, false_block) ->
      sprintf
        "br %s, %s, %s"
        (render_value tcx cond)
        (render_value tcx true_block)
        (render_value tcx false_block)
  | Jmp bb -> sprintf "jmp %s" (render_value tcx bb)
  | Ret ret -> sprintf "ret %s" (render_value tcx ret)
  | RetUnit -> "ret"
;;

let render_inst tcx inst : string =
  (if has_value inst then sprintf "    %%%d = " inst.id else "    ")
  ^
  match inst.kind with
  | Alloca ty -> sprintf "alloca %s" (tcx#render_ty ty)
  | Binary (kind, left, right) ->
      sprintf
        "%s %s, %s"
        (render_binary kind)
        (render_value tcx left)
        (render_value tcx right)
  | Phi (ty, args) ->
      sprintf
        "phi %s, %s"
        (tcx#render_ty ty)
        (args#join ", " (fun (bb, inst) ->
             sprintf "[%s, %s]" (render_value tcx bb) (render_value tcx inst)))
  | Aggregate (Adt (did, vidx, subst), values) ->
      let variants = tcx#variants (tcx#adt did subst) |> Option.get in
      let (Variant variant) = variants#get vidx in
      let path, _ = tcx#into_segments variant.def_id in
      let name = String.concat "::" path in
      sprintf "%s { %s }" name (values#join ", " (render_value tcx))
  | Aggregate ((Slice ty | Array ty | Repeat (ty, _)), values) ->
      let name = tcx#render_ty ty in
      sprintf "%s { %s }" name (values#join ", " (render_value tcx))
  | Discriminant value -> sprintf "discriminant %s" (render_value tcx value)
  | Payload (value, idx) ->
      let (Variant v) =
        (tcx#variants (get_ty tcx value) |> Option.get)#get idx
      in
      let segments, _ = tcx#into_segments v.def_id in
      let name = segments |> List.rev |> List.hd in
      (* sprintf "payload %s <{%d}>" (render_value tcx value) idx *)
      sprintf "%s as %s" (render_value tcx value) name
  | Store (src, dst) ->
      sprintf "store %s, %s" (render_value tcx src) (render_value tcx dst)
  | Copy ptr ->
      sprintf
        "copy %s, %s"
        (match !(get_ty tcx ptr) with
         | Ptr (_, ty) -> tcx#render_ty ty
         | Ref (_, ty) -> tcx#render_ty ty
         | _ -> assert false)
        (render_value tcx ptr)
  | Move ptr ->
      sprintf
        "move %s, %s"
        (match !(get_ty tcx ptr) with
         | Ptr (_, ty) -> tcx#render_ty ty
         | Ref (_, ty) -> tcx#render_ty ty
         | _ -> assert false)
        (render_value tcx ptr)
  | Gep (ty, value, index) ->
      let ty =
        match !ty with
        | Adt _ ->
            let (Variant variant) = tcx#non_enum_variant ty |> Option.get in
            let (Field { ty; _ }) = variant.fields#get index in
            ty
        | Tuple tys -> tys#get index
        | _ -> assert false
      in
      sprintf
        "gep %s, %s, %d"
        (tcx#render_ty ty)
        (render_value tcx value)
        index
  | Index (ty, value, idx) ->
      let ty = tcx#slice_inner ty in
      sprintf
        "%s, %s[%s]"
        (tcx#render_ty ty)
        (render_value tcx value)
        (render_value tcx idx)
  | Len value -> sprintf "length %s" (render_value tcx value)
  | Call (ty, fn, args) ->
      let ty = Fn.ret tcx ty in
      sprintf
        "call %s %s(%s)"
        (tcx#render_ty ty)
        (render_value tcx fn)
        (args#join ", " (tcx |> render_value))
  | Intrinsic (name, args) ->
      sprintf "intrinsic %s(%s)" name (args#join ", " (tcx |> render_value))
  | BitCast (value, ty) ->
      sprintf "bitcast %s to %s" (render_value tcx value) (tcx#render_ty ty)
  | Zext (value, ty) ->
      sprintf "zext %s to %s" (render_value tcx value) (tcx#render_ty ty)
  | Trunc (value, ty) ->
      sprintf "trunc %s to %s" (render_value tcx value) (tcx#render_ty ty)
  | PtrToInt (value, ty) ->
      sprintf "ptrtoint %s to %s" (render_value tcx value) (tcx#render_ty ty)
  | IntToPtr (value, ty) ->
      sprintf "inttoptr %s to %s" (render_value tcx value) (tcx#render_ty ty)
  | Coercion (ArrayToSlice, value, ty) ->
      sprintf
        "coercion[array => slice], %s to %s"
        (render_value tcx value)
        (tcx#render_ty ty)
  | Trap _ -> "trap"
  | Nop -> "nop"
;;

open Metadata.Encoder
open Metadata.Decoder

let rec encode enc { kind; ty; id; _ } =
  ignore (encode_inst_kind enc kind);
  Ty.encode enc ty;
  enc#emit_usize id

and encode_inst_kind enc kind =
  let disc = inst_kind_to_enum kind in
  kind
  |> function
  | Alloca ty -> enc#emit_with disc (fun _ -> Ty.encode enc ty)
  | Binary (kind, left, right) ->
      enc#emit_with disc (fun _ ->
          enc#emit_usize (binary_kind_to_enum kind);
          encode_value enc left;
          encode_value enc right)
  | Phi (ty, values) ->
      enc#emit_with disc (fun _ ->
          Ty.encode enc ty;
          encode_vec enc values (fun _ (v1, v2) ->
              encode_value enc v1;
              encode_value enc v2))
  | Aggregate (Adt (def_id, i, subst), values) ->
      enc#emit_with disc (fun _ ->
          enc#emit_usize 0;
          Def_id.encode enc def_id;
          enc#emit_usize i;
          encode_subst enc subst;
          encode_vec enc values encode_value)
  | Aggregate (Slice ty, values) ->
      enc#emit_with disc (fun _ ->
          enc#emit_usize 1;
          Ty.encode enc ty;
          encode_vec enc values encode_value)
  | Aggregate (Array ty, values) ->
      enc#emit_with disc (fun _ ->
          enc#emit_usize 2;
          Ty.encode enc ty;
          encode_vec enc values encode_value)
  | Aggregate (Repeat (ty, size), values) ->
      enc#emit_with disc (fun _ ->
          enc#emit_usize 3;
          Ty.encode enc ty;
          enc#emit_usize size;
          encode_vec enc values encode_value)
  | Discriminant value | Copy value | Move value | Len value ->
      enc#emit_with disc (fun _ -> encode_value enc value)
  | Payload (value, i) ->
      enc#emit_with disc (fun _ ->
          encode_value enc value;
          enc#emit_usize i)
  | Store (src, dst) ->
      enc#emit_with disc (fun _ ->
          encode_value enc src;
          encode_value enc dst)
  | Gep (ty, value, i) ->
      enc#emit_with disc (fun _ ->
          Ty.encode enc ty;
          encode_value enc value;
          enc#emit_usize i)
  | Call (ty, value, args) ->
      enc#emit_with disc (fun _ ->
          Ty.encode enc ty;
          encode_value enc value;
          encode_vec enc args encode_value)
  | Intrinsic (name, args) ->
      enc#emit_with disc (fun _ ->
          enc#emit_str name;
          encode_vec enc args encode_value)
  | Trap msg -> enc#emit_with disc (fun _ -> enc#emit_str msg)
  | BitCast (v, ty)
  | Zext (v, ty)
  | Trunc (v, ty)
  | PtrToInt (v, ty)
  | IntToPtr (v, ty) ->
      enc#emit_with disc (fun _ ->
          encode_value enc v;
          Ty.encode enc ty)
  | Nop -> enc#emit_usize (Int64.to_int disc)
  | Index (ty, v, idx) ->
      enc#emit_with disc (fun _ ->
          Ty.encode enc ty;
          encode_value enc v;
          encode_value enc idx)
  | Coercion (ArrayToSlice, v, ty) ->
      enc#emit_with disc (fun _ ->
          enc#emit_usize 0;
          encode_value enc v;
          Ty.encode enc ty)

and encode_terminator enc = function
  | Switch (d, br, default) ->
      enc#emit_with 0L (fun _ ->
          encode_value enc d;
          encode_vec enc br (fun _ (v1, v2) ->
              encode_value enc v1;
              encode_value enc v2);
          encode_option enc default encode_value)
  | Br (cond, tb, fb) ->
      enc#emit_with 1L (fun _ ->
          encode_value enc cond;
          encode_value enc tb;
          encode_value enc fb)
  | Jmp value -> enc#emit_with 2L (fun _ -> encode_value enc value)
  | Ret value -> enc#emit_with 3L (fun _ -> encode_value enc value)
  | RetUnit -> enc#emit_usize 4

and encode_value enc : value -> unit = function
  | Const { kind; ty } ->
      enc#emit_with 0L (fun _ ->
          encode_const_kind enc kind;
          Ty.encode enc ty)
  | VReg { id; _ } -> enc#emit_with 1L (fun _ -> enc#emit_usize id)
  | Label { bid; _ } -> enc#emit_with 2L (fun _ -> enc#emit_usize bid)
  | Param (ty, name, id) ->
      enc#emit_with 3L (fun e ->
          Ty.encode e ty;
          e#emit_str name;
          e#emit_usize id)
  | Global (Fn instance) ->
      enc#emit_with 4L (fun _ -> encode_instance enc instance)

and encode_const_kind enc = function
  | Int i -> enc#emit_with 0L (fun _ -> enc#emit_usize i)
  | Float _ -> enc#emit_with 1L (fun _ -> assert false)
  | Str s -> enc#emit_with 2L (fun _ -> enc#emit_str s)
  | Bool b -> enc#emit_with 3L (fun _ -> enc#emit_bool b)
  | Struct values ->
      enc#emit_with 4L (fun _ -> encode_vec enc values encode_value)
  | Unit -> enc#emit_usize 5
;;

let _insts = ref (new vec)
let _bbs = ref (new vec)

let rec decode tcx dec =
  let kind = decode_inst_kind tcx dec in
  let ty = Ty.decode tcx dec in
  let id = dec#read_usize in
  let inst = { kind; ty; id; span = Span.dummy } in
  if has_value inst then !_insts#push inst;
  inst

and decode_inst_kind tcx dec =
  match dec#read_usize with
  | 0 -> Alloca (Ty.decode tcx dec)
  | 1 ->
      let kind = binary_kind_of_enum dec#read_usize |> Option.get in
      let left = decode_value tcx dec in
      let right = decode_value tcx dec in
      Binary (kind, left, right)
  | 2 ->
      let ty = Ty.decode tcx dec in
      let values = new vec in
      decode_vec dec values (fun dec ->
          let v1 = decode_value tcx dec in
          let v2 = decode_value tcx dec in
          v1, v2);
      Phi (ty, values)
  | 3 ->
      let agg =
        match dec#read_usize with
        | 0 ->
            let def_id = Def_id.decode dec in
            let i = dec#read_usize in
            let subst = decode_subst tcx dec in
            Adt (def_id, i, subst)
        | 1 -> Slice (Ty.decode tcx dec)
        | _ -> assert false
      in
      let values = new vec in
      decode_vec dec values (decode_value tcx);
      Aggregate (agg, values)
  | 4 -> Discriminant (decode_value tcx dec)
  | 5 ->
      let v = decode_value tcx dec in
      let i = dec#read_usize in
      Payload (v, i)
  | 6 ->
      let src = decode_value tcx dec in
      let dst = decode_value tcx dec in
      Store (src, dst)
  | 7 -> Copy (decode_value tcx dec)
  | 8 -> Move (decode_value tcx dec)
  | 9 ->
      let ty = Ty.decode tcx dec in
      let value = decode_value tcx dec in
      let i = dec#read_usize in
      Gep (ty, value, i)
  | 10 ->
      let ty = Ty.decode tcx dec in
      let value = decode_value tcx dec in
      let args = new vec in
      decode_vec dec args (decode_value tcx);
      Call (ty, value, args)
  | 11 ->
      let name = dec#read_str in
      let args = new vec in
      decode_vec dec args (decode_value tcx);
      Intrinsic (name, args)
  | 12 -> Trap dec#read_str
  | 13 ->
      let v = decode_value tcx dec in
      let ty = Ty.decode tcx dec in
      BitCast (v, ty)
  | 14 ->
      let v = decode_value tcx dec in
      let ty = Ty.decode tcx dec in
      Zext (v, ty)
  | 15 ->
      let v = decode_value tcx dec in
      let ty = Ty.decode tcx dec in
      Trunc (v, ty)
  | 16 ->
      let v = decode_value tcx dec in
      let ty = Ty.decode tcx dec in
      PtrToInt (v, ty)
  | 17 ->
      let v = decode_value tcx dec in
      let ty = Ty.decode tcx dec in
      IntToPtr (v, ty)
  | 18 -> Nop
  | 19 ->
      let ty = Ty.decode tcx dec in
      let v = decode_value tcx dec in
      let idx = decode_value tcx dec in
      Index (ty, v, idx)
  | 20 -> Len (decode_value tcx dec)
  | 21 ->
      let i = dec#read_usize in
      assert (i = 0);
      let v = decode_value tcx dec in
      let ty = Ty.decode tcx dec in
      Coercion (ArrayToSlice, v, ty)
  | _ -> assert false

and decode_terminator tcx dec =
  match dec#read_usize with
  | 0 ->
      let v = decode_value tcx dec in
      let br = new vec in
      decode_vec dec br (fun _ ->
          let v1 = decode_value tcx dec in
          let v2 = decode_value tcx dec in
          v1, v2);
      let default = decode_option dec (decode_value tcx) in
      Switch (v, br, default)
  | 1 ->
      let cond = decode_value tcx dec in
      let tb = decode_value tcx dec in
      let fb = decode_value tcx dec in
      Br (cond, tb, fb)
  | 2 -> Jmp (decode_value tcx dec)
  | 3 -> Ret (decode_value tcx dec)
  | 4 -> RetUnit
  | _ -> assert false

and decode_value tcx dec =
  match dec#read_usize with
  | 0 -> Const { ty = assert false; kind = assert false }
  | 1 ->
      let i = dec#read_usize in
      let i = !_insts#get i in
      VReg i
  | 2 ->
      let i = dec#read_usize in
      let bb = !_bbs#get i in
      Label bb
  | 3 ->
      let ty = Ty.decode tcx dec in
      let name = dec#read_str in
      let id = dec#read_usize in
      Param (ty, name, id)
  | 4 -> Global (Fn (decode_instance tcx dec))
  | _ -> assert false
;;
