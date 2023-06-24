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

and inst_kind =
  | Alloca of ty
  (* TODO: add basic block type to value *)
  | Br of value * value * value
  | Store of value * value
  | Load of value
  | Ret of value
  | RetUnit
  | Nop

and value =
  | Const of const * ty
  | VReg of t * int * ty
  | Label of int

and const =
  | Int of int
  | Float of float
[@@deriving show]

let has_value = function
  | Store _ | Ret _ | RetUnit | Nop -> false
  | _ -> true

let render_const = function
  | Int value -> sprintf "%d" value
  | Float value -> sprintf "%f" value

let render_value = function
  | Const (const, ty) ->
      sprintf "%s %s" (Fmt.render_ty ty) (render_const const)
  | VReg (_, i, ty) -> sprintf "%s%%%i" (Fmt.render_ty ty ^ " ") i
  | Label i -> sprintf "%%%d" i

let get_ty = function
  | Const (_, ty) | VReg (_, _, ty) -> ty
  | _ -> assert false

let render_inst inst : string =
  (if has_value inst.kind then sprintf "    %%%d = " inst.id else "    ")
  ^
  match inst.kind with
  | Alloca ty -> sprintf "alloca %s" (Fmt.render_ty ty)
  | Br (cond, true_block, false_block) ->
      sprintf "br %s, %s, %s" (render_value cond) (render_value true_block)
        (render_value false_block)
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
