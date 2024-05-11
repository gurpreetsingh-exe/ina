open Ty
open Def_id
open Structures.Vec
open Structures.Hashmap

type variable = {
    index: int
  ; ty: ty ref
}

type body = {
    bindings: (def_id, variable) hashmap
  ; index: int
  ; mutable is_or: bool
}

type cons =
  | Cons of (ty ref * int)
  | Int of int
  | True
  | False
  | Range of (int * int)

type t =
  | Success of body
  | Failure
  | Switch of (variable * case vec * t option)

and case = {
    cons: cons
  ; args: variable vec
  ; body: t
}
