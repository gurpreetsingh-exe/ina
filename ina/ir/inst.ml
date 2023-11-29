open Middle.Ty
open Middle.Def_id
open Printf
open Structures.Vec

type t = {
    kind: inst_kind
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
  | Br of value * value * value
  | Jmp of value
  (* (bb * inst) *)
  | Phi of ty * (value * value) list
  | Store of value * value
  | Load of value
  | Gep of ty * value * int
  | Call of ty ref * value * value vec
  | Intrinsic of string * value list
  | Ret of value
  | Trap of value
  (* Bitcast of ty * value *)
  (* Zext of ty * value *)
  (* Sext of ty * value *)
  (* Trunc of ty * value *)
  (* Fpext of ty * value *)
  (* Fptrunc of ty * value *)
  | PtrToInt of value * ty
  | IntToPtr of value * ty
  | RetUnit
  | Nop

and value =
  | Const of {
        kind: const_kind
      ; ty: ty ref
    }
  | VReg of t
  | Label of basic_block
  | Param of ty * string * int
  | Global of def_id

and const_kind =
  | Int of int
  | Float of float
  | Str of string
  | Bool of bool
  | Struct of value list

and basic_block = {
    mutable pred: basic_block vec
  ; mutable succ: basic_block vec
  ; mutable insts: t vec
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
  | BitAnd | And -> And
  | BitOr | Or -> Or
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

let has_value = function
  | Br _ | Jmp _ | Store _ | Ret _ | RetUnit | Nop -> false
  | _ -> true
;;

let rec render_const tcx = function
  | Int value -> sprintf "%d" value
  | Float value -> sprintf "%f" value
  | Str value -> sprintf "%s" (String.escaped value)
  | Bool value -> sprintf "%b" value
  | Struct values ->
      sprintf
        "{ %s }"
        (String.concat ", " (List.map (tcx |> render_value) values))

and render_value tcx = function
  | Const const ->
      sprintf "%s %s" (render_ty !(const.ty)) (render_const tcx const.kind)
  | VReg inst -> sprintf "%s %%%i" (render_ty !(inst.ty)) inst.id
  | Label bb -> sprintf "label %%bb%d" bb.bid
  | Param (ty, name, _) -> sprintf "%s %%%s" (render_ty ty) name
  | Global id ->
      sprintf
        "@%s"
        ((tcx#def_id_to_qpath#unsafe_get id)#join "::" (fun s -> s))
;;

let get_ty tcx = function
  | Param (ty, _, _) -> tcx#intern ty
  | Const c -> c.ty
  | VReg v -> v.ty
  | Global id -> tcx#node_id_to_ty#unsafe_get id.inner
  | _ -> assert false
;;

let extract_block = function Label bb -> bb | _ -> assert false

let render_inst tcx inst : string =
  (if has_value inst.kind then sprintf "    %%%d = " inst.id else "    ")
  ^
  match inst.kind with
  | Alloca ty -> sprintf "alloca %s" (render_ty !ty)
  | Binary (kind, left, right) ->
      sprintf
        "%s %s, %s"
        (render_binary kind)
        (render_value tcx left)
        (render_value tcx right)
  | Br (cond, true_block, false_block) ->
      sprintf
        "br %s, %s, %s"
        (render_value tcx cond)
        (render_value tcx true_block)
        (render_value tcx false_block)
  | Phi (ty, args) ->
      sprintf
        "phi %s, %s"
        (render_ty ty)
        (String.concat
           ", "
           (List.map
              (fun (bb, inst) ->
                sprintf
                  "[%s, %s]"
                  (render_value tcx bb)
                  (render_value tcx inst))
              args))
  | Jmp bb -> sprintf "jmp %s" (render_value tcx bb)
  | Store (src, dst) ->
      sprintf "store %s, %s" (render_value tcx src) (render_value tcx dst)
  | Load ptr ->
      sprintf
        "load %s, %s"
        (match !(get_ty tcx ptr) with
         | Ptr ty -> render_ty ty
         | Ref ty -> render_ty ty
         | _ -> assert false)
        (render_value tcx ptr)
  | Gep (_, value, index) ->
      sprintf "gep %s, %d" (render_value tcx value) index
  | Call (ty, fn, args) ->
      let ty =
        match !ty with FnPtr { ret; _ } -> ret | _ -> assert false
      in
      sprintf
        "call %s %s(%s)"
        (render_ty ty)
        (render_value tcx fn)
        (args#join ", " (tcx |> render_value))
  | Intrinsic (name, args) ->
      sprintf
        "intrinsic %s(%s)"
        name
        (String.concat ", " (List.map (tcx |> render_value) args))
  | PtrToInt (value, ty) ->
      sprintf "ptrtoint %s to %s" (render_value tcx value) (render_ty ty)
  | IntToPtr (value, ty) ->
      sprintf "inttoptr %s to %s" (render_value tcx value) (render_ty ty)
  | Ret ret -> sprintf "ret %s" (render_value tcx ret)
  | Trap _ -> "trap"
  | RetUnit -> "ret"
  | Nop -> "nop"
;;