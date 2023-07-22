open Ty
open Front
open Printf

let pp_ty (fmt : Format.formatter) ty : unit =
  let open Front in
  Format.pp_print_string fmt (Fmt.render_ty ty)

type t = {
  kind : inst_kind;
  id : int;
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
  | Alloca of ty
  | Binary of binary_kind * value * value
  | Br of value * value * value
  | Jmp of value
  (* (bb * inst) *)
  | Phi of ty * (value * value) list
  | Store of value * value
  | Load of value
  | Gep of ty * value * int
  | Call of ty * value * value list
  | Intrinsic of string * value list
  | Ret of value
  | Trap of value
  (* Bitcast of ty * value *)
  (* Zext of ty * value *)
  (* Sext of ty * value *)
  (* Trunc of ty * value *)
  (* Fpext of ty * value *)
  (* Fptrunc of ty * value *)
  | RetUnit
  | Nop

and value =
  | Const of const * ty
  | VReg of t * int * ty
  | Label of basic_block
  | Param of ty * string * int
  | Global of string

and const =
  | Int of int
  | Float of float
  | Str of string
  | Bool of bool
  | Struct of value list

and basic_block = {
  mutable pred : basic_block list;
  mutable succ : basic_block list;
  mutable insts : t list;
  mutable bid : int;
  mutable is_entry : bool;
}
[@@deriving show]

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

let has_value = function
  | Br _ | Jmp _ | Store _ | Ret _ | RetUnit | Nop -> false
  | _ -> true

let rec render_const = function
  | Int value -> sprintf "%d" value
  | Float value -> sprintf "%f" value
  | Str value -> sprintf "%s" (String.escaped value)
  | Bool value -> sprintf "%b" value
  | Struct values ->
      sprintf "{ %s }" (String.concat ", " (List.map render_value values))

and render_value = function
  | Const (const, ty) ->
      sprintf "%s %s" (Fmt.render_ty ty) (render_const const)
  | VReg (_, i, ty) -> sprintf "%s%%%i" (Fmt.render_ty ty ^ " ") i
  | Label bb -> sprintf "label %%bb%d" bb.bid
  | Param (ty, name, _) -> sprintf "%s %%%s" (Fmt.render_ty ty) name
  | Global name -> sprintf "@%s" name

let get_ty = function
  | Param (ty, _, _) | Const (_, ty) | VReg (_, _, ty) -> ty
  | _ -> assert false

let extract_block = function Label bb -> bb | _ -> assert false

let render_inst inst : string =
  (if has_value inst.kind then sprintf "    %%%d = " inst.id else "    ")
  ^
  match inst.kind with
  | Alloca ty -> sprintf "alloca %s" (Fmt.render_ty ty)
  | Binary (kind, left, right) ->
      sprintf "%s %s, %s" (render_binary kind) (render_value left)
        (render_value right)
  | Br (cond, true_block, false_block) ->
      sprintf "br %s, %s, %s" (render_value cond) (render_value true_block)
        (render_value false_block)
  | Phi (ty, args) ->
      sprintf "phi %s, %s" (Fmt.render_ty ty)
        (String.concat ", "
           (List.map
              (fun (bb, inst) ->
                sprintf "[%s, %s]" (render_value bb) (render_value inst))
              args))
  | Jmp bb -> sprintf "jmp %s" (render_value bb)
  | Store (src, dst) ->
      sprintf "store %s, %s" (render_value src) (render_value dst)
  | Load ptr ->
      sprintf "load %s, %s"
        (match get_ty ptr with
        | Ptr ty -> Fmt.render_ty ty
        | RefTy ty -> Fmt.render_ty ty
        | _ -> assert false)
        (render_value ptr)
  | Gep (_, value, index) -> sprintf "gep %s, %d" (render_value value) index
  | Call (ty, fn, args) ->
      let ty =
        match ty with FnTy (_, ret_ty, _) -> ret_ty | _ -> assert false
      in
      sprintf "call %s %s(%s)" (Fmt.render_ty ty) (render_value fn)
        (String.concat ", " (List.map render_value args))
  | Intrinsic (name, args) ->
      sprintf "intrinsic %s(%s)" name
        (String.concat ", " (List.map render_value args))
  | Ret ret -> sprintf "ret %s" (render_value ret)
  | Trap _ -> "trap"
  | RetUnit -> "ret"
  | Nop -> "nop"
