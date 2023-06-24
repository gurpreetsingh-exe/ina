open Ast
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

and inst_kind =
  | Alloca of ty
  | Binary of binary_kind * value * value
  | Br of value * value * value
  | Jmp of value
  | Phi of ty * (value * value) list
  | Store of value * value
  | Load of value
  | Ret of value
  | RetUnit
  | Nop

and value =
  | Const of const * ty
  | VReg of t * int * ty
  | Label of basic_block

and const =
  | Int of int
  | Float of float

and basic_block = {
  mutable pred : basic_block list;
  mutable succ : basic_block list;
  mutable insts : t list;
  mutable bid : int;
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

let has_value = function
  | Br _ | Jmp _ | Store _ | Ret _ | RetUnit | Nop -> false
  | _ -> true

let render_const = function
  | Int value -> sprintf "%d" value
  | Float value -> sprintf "%f" value

let render_value = function
  | Const (const, ty) ->
      sprintf "%s %s" (Fmt.render_ty ty) (render_const const)
  | VReg (_, i, ty) -> sprintf "%s%%%i" (Fmt.render_ty ty ^ " ") i
  | Label bb -> sprintf "label %%bb%d" bb.bid

let get_ty = function
  | Const (_, ty) | VReg (_, _, ty) -> ty
  | _ -> assert false

let render_inst inst : string =
  (if has_value inst.kind then sprintf "    %%%d = " inst.id else "    ")
  ^
  match inst.kind with
  | Alloca ty -> sprintf "alloca %s" (Fmt.render_ty ty)
  | Binary (_, left, right) ->
      sprintf "binary %s, %s" (render_value left) (render_value right)
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
  | Store (dst, src) ->
      sprintf "store %s, %s" (render_value dst) (render_value src)
  | Load ptr ->
      sprintf "load %s, %s"
        (match get_ty ptr with
        | Ptr ty -> Fmt.render_ty ty
        | _ -> assert false)
        (render_value ptr)
  | Ret ret -> sprintf "ret %s" (render_value ret)
  | RetUnit -> "ret"
  | Nop -> "nop"
