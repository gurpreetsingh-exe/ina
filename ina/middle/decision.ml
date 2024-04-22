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
}

type cons =
  | Cons of (ty ref * int)
  | Int of int

type t =
  | Success of body
  | Failure
  | Switch of (variable * case vec * t option)

and case = {
    cons: cons
  ; args: variable vec
  ; body: t
}
