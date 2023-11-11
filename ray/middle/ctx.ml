open Ty
open Printf
open Structures.Hashmap
open Def_id

class tcx =
  object
    val type_ = new hashmap
    method create_def (id : def_id) (ty : ty) = type_#insert id ty
  end
