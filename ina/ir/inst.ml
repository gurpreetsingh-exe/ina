open Middle.Ty
open Middle.Def_id
open Printf
open Structures.Vec

type t = {
    mutable kind: inst_kind
  ; ty: ty ref
  ; mutable id: int
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
  | Store of value * value
  | Copy of value
  | Move of value
  | Gep of ty ref * value * int
  | Call of ty ref * value * value vec
  | Intrinsic of string * value list
  | Trap of value
  | BitCast of value * ty ref
  (* Zext of ty * value *)
  (* Sext of ty * value *)
  (* Trunc of ty * value *)
  (* Fpext of ty * value *)
  (* Fptrunc of ty * value *)
  | PtrToInt of value * ty
  | IntToPtr of value * ty
  | Nop

and terminator =
  | Br of value * value * value
  | Jmp of value
  | Ret of value
  | RetUnit

and value =
  | Const of {
        kind: const_kind
      ; ty: ty ref
    }
  | VReg of t
  | Label of basic_block
  | Param of ty ref * string * int
  | Global of def_id

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
  | Global id ->
      sprintf
        "@%s"
        ((tcx#def_id_to_qpath#unsafe_get id)#join "::" (fun s -> s))
;;

let get_ty tcx = function
  | Param (ty, _, _) -> ty
  | Const c -> c.ty
  | VReg v -> v.ty
  | Global id -> tcx#def_id_to_ty#unsafe_get id
  | _ -> assert false
;;

let extract_block = function Label bb -> bb | _ -> assert false

let render_terminator tcx term =
  "    "
  ^
  match term with
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
  | Store (src, dst) ->
      sprintf "store %s, %s" (render_value tcx src) (render_value tcx dst)
  | Copy ptr ->
      sprintf
        "copy %s, %s"
        (match !(get_ty tcx ptr) with
         | Ptr ty -> tcx#render_ty ty
         | Ref ty -> tcx#render_ty ty
         | _ -> assert false)
        (render_value tcx ptr)
  | Move ptr ->
      sprintf
        "move %s, %s"
        (match !(get_ty tcx ptr) with
         | Ptr ty -> tcx#render_ty ty
         | Ref ty -> tcx#render_ty ty
         | _ -> assert false)
        (render_value tcx ptr)
  | Gep (ty, value, index) ->
      let (Variant variant) = tcx#non_enum_variant ty in
      let (Field { ty; _ }) = variant.fields#get index in
      sprintf
        "gep %s, %s, %d"
        (tcx#render_ty ty)
        (render_value tcx value)
        index
  | Call (ty, fn, args) ->
      let ty =
        match !ty with FnPtr { ret; _ } -> ret | _ -> assert false
      in
      sprintf
        "call %s %s(%s)"
        (tcx#render_ty ty)
        (render_value tcx fn)
        (args#join ", " (tcx |> render_value))
  | Intrinsic (name, args) ->
      sprintf
        "intrinsic %s(%s)"
        name
        (String.concat ", " (List.map (tcx |> render_value) args))
  | BitCast (value, ty) ->
      sprintf "bitcast %s to %s" (render_value tcx value) (tcx#render_ty ty)
  | PtrToInt (value, ty) ->
      sprintf
        "ptrtoint %s to %s"
        (render_value tcx value)
        (tcx#render_ty (ref ty))
  | IntToPtr (value, ty) ->
      sprintf
        "inttoptr %s to %s"
        (render_value tcx value)
        (tcx#render_ty (ref ty))
  | Trap _ -> "trap"
  | Nop -> "nop"
;;
