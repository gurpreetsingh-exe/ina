open Middle.Ctx
open Middle.Ty
open Structures.Vec
open Unification_table
open Utils.Panic

type ty_constraint = TypeEqual of ty * ty

module IntVid :
  UnifyKey
    with type k = intvid
     and type v = int_ty option
     and type e = int_ty * int_ty = struct
  type k = intvid
  type v = int_ty option
  type e = int_ty * int_ty

  let index (vid : k) = vid.index
  let from_index index : k = { index }
  let tag () = "IntVid"
  let order_roots _k1 _v1 _k2 _v2 = None
  let display_key key = display_intvid key

  let display_value = function
    | Some ty -> display_int_ty ty
    | None -> "None"
  ;;

  let unify_values (v1 : v) (v2 : v) =
    match v1, v2 with
    | None, None -> Ok None
    | Some v, None | None, Some v -> Ok (Some v)
    | Some v1, Some v2 when v1 = v2 -> Ok (Some v1)
    | Some v1, Some v2 -> Error (v1, v2)
  ;;
end

module FloatVid :
  UnifyKey
    with type k = floatvid
     and type v = float_ty option
     and type e = float_ty * float_ty = struct
  type k = floatvid
  type v = float_ty option
  type e = float_ty * float_ty

  let index (vid : k) = vid.index
  let from_index index : k = { index }
  let tag () = "FloatVid"
  let order_roots _k1 _v1 _k2 _v2 = None
  let display_key key = display_floatvid key

  let display_value = function
    | Some ty -> display_float_ty ty
    | None -> "None"
  ;;

  let unify_values (v1 : v) (v2 : v) =
    match v1, v2 with
    | None, None -> Ok None
    | Some v, None | None, Some v -> Ok (Some v)
    | Some v1, Some v2 when v1 = v2 -> Ok (Some v1)
    | Some v1, Some v2 -> Error (v1, v2)
  ;;
end

module IntUt = Unification_table (IntVid)
module FloatUt = Unification_table (FloatVid)

type infer_ctx = {
    tcx: tcx
  ; int_ut: IntUt.t
  ; float_ut: FloatUt.t
  ; constraints: ty_constraint vec
}

let infer_ctx_create tcx =
  {
    tcx
  ; int_ut = IntUt.create ()
  ; float_ut = FloatUt.create ()
  ; constraints = new vec
  }
;;

let infcx_new_int_var infcx =
  infcx.tcx#intern (Infer (IntVar (IntUt.new_key infcx.int_ut None)))
;;

let infcx_new_float_var infcx =
  infcx.tcx#intern (Infer (FloatVar (FloatUt.new_key infcx.float_ut None)))
;;

let fold_infer_ty infcx v =
  match v with
  | IntVar tyvid ->
      IntUt.probe_value infcx.int_ut tyvid
      |> Option.map (fun v ->
             let ty = infcx.tcx#int_ty_to_ty v in
             dbg
               "fold_infer_ty(%s) = %s\n"
               (display_intvid tyvid)
               (render_ty2 ty);
             ty)
  | FloatVar tyvid ->
      FloatUt.probe_value infcx.float_ut tyvid
      |> Option.map (fun v ->
             let ty = infcx.tcx#float_ty_to_ty v in
             dbg
               "fold_infer_ty(%s) = %s\n"
               (display_floatvid tyvid)
               (render_ty2 ty);
             ty)
  | TyVar _ -> None
;;

let rec fold_ty infcx ty =
  match !ty with
  | Infer i ->
      (match fold_infer_ty infcx i with Some ty -> ty | None -> ty)
  | Ptr ty -> infcx.tcx#ptr (fold_ty infcx ty)
  | Ref ty -> infcx.tcx#ref (fold_ty infcx ty)
  | _ -> ty
;;

let resolve_vars infcx ty = fold_ty infcx ty
