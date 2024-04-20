open Middle
open Middle.Ty
open Middle.Def_id
open Printf
open Structures.Vec
open Source

type instance_def =
  | Fn of def_id
  | Intrinsic of def_id

and instance = {
    def: instance_def
  ; subst: subst
}

and item = Fn of instance

let encode_instance enc { def; subst } =
  (match def with
   | Fn did -> enc#emit_with 0L (fun e -> Def_id.encode e did)
   | Intrinsic did -> enc#emit_with 1L (fun e -> Def_id.encode e did));
  Ty.encode_subst enc subst
;;

let decode_instance tcx dec =
  let def =
    match dec#read_usize with
    | 0 -> Fn (Def_id.decode dec)
    | 1 -> Intrinsic (Def_id.decode dec)
    | _ -> assert false
  in
  let subst = Ty.decode_subst tcx dec in
  { def; subst }
;;

let instance_def_id instance =
  instance.def |> function Fn id | Intrinsic id -> id
;;

let render_instance (tcx : Middle.Ctx.tcx) instance =
  match instance.def with
  | Fn id | Intrinsic id ->
      let (Subst subst) = instance.subst in
      sprintf
        "%s@%s%s"
        (match instance.def with Intrinsic _ -> "i" | _ -> "")
        (tcx#into_segments id |> function
         | segments, _ -> segments |> String.concat "::")
        (if subst#empty
         then ""
         else
           sprintf
             "[%s]"
             (subst#join ", " (function Ty ty -> tcx#render_ty ty)))
;;

type t = {
    mutable kind: inst_kind
  ; ty: ty ref
  ; mutable id: int
  ; span: Span.t
}

and binary_kind =
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
  | Nop

and terminator =
  | Switch of (value * (value * value) vec)
  | Br of value * value * value
  | Jmp of value
  | Ret of value
  | RetUnit

and aggregate = Adt of (def_id * int * subst)

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
  ; mutable is_entry: bool
}
(* [@@deriving show] *)

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
  | Label bb -> sprintf "label %%bb%d" bb.bid
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
        (match def with Fn id | Intrinsic id -> tcx#get_def id)
        subst
  | _ -> assert false
;;

let extract_block = function Label bb -> bb | _ -> assert false

let render_terminator tcx term =
  "    "
  ^
  match term with
  | Switch (cond, args) ->
      sprintf
        "switch %s, [%s]"
        (render_value tcx cond)
        (args#join ", " (fun (bb, inst) ->
             sprintf "%s: %s" (render_value tcx bb) (render_value tcx inst)))
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
  | Trap _ -> "trap"
  | Nop -> "nop"
;;

let encode enc = function _ -> assert false
let encode_value enc = function _ -> assert false
let decode dec = assert false
let decode_value dec = assert false
