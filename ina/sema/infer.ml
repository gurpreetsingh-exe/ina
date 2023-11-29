open Ast
open Printf
open Errors
open Source
open Session
open Middle.Ctx
open Middle.Ty
open Diagnostic
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
             dbg "fold_infer_ty(int) = %s\n" (render_ty !ty);
             ty)
  | FloatVar tyvid ->
      FloatUt.probe_value infcx.float_ut tyvid
      |> Option.map (fun v ->
             let ty = infcx.tcx#float_ty_to_ty v in
             dbg "fold_infer_ty(float) = %s\n" (render_ty !ty);
             ty)
  | TyVar _ -> None
;;

let fold_ty infcx ty =
  match !ty with
  | Infer i ->
      (match fold_infer_ty infcx i with Some ty -> ty | None -> ty)
  | _ -> ty
;;

let resolve_vars infcx ty = fold_ty infcx ty

type infer_err =
  | MismatchTy of ty * ty
  | FnNotFound of ident
  | StructNotFound of ident
  | VarNotFound of ident
  | MismatchArgs of int * int
  | InvalidCall of ty
  | InvalidDeref of ty
  | AssocFnAsMethod of string

let mismatch_ty expected ty span =
  let msg =
    sprintf "expected `%s`, found `%s`" (render_ty expected) (render_ty ty)
  in
  mk_err msg span
;;

let item_not_found item name span =
  let msg = sprintf "%s `%s` is not found in this scope" item name in
  mk_err msg span
;;

let fn_not_found name span = item_not_found "function" name span
let local_var_not_found name span = item_not_found "local variable" name span
let struct_not_found name span = item_not_found "struct" name span

let invalid_deref ty span =
  let msg = sprintf "`%s` cannot be dereferenced" (render_ty ty) in
  mk_err msg span
;;

let invalid_call ty span =
  let msg = sprintf "`%s` is not callable" (render_ty ty) in
  mk_err msg span
;;

let mismatch_args expected found span =
  let msg =
    sprintf
      "expected %d arg%s, found %d"
      expected
      (if expected = 1 then "" else "s")
      found
  in
  mk_err msg span
;;

let assoc_call_as_method name span =
  let msg =
    sprintf "`%s` is an associated function, not a method call" name
  in
  mk_err msg span
;;

let infer_err_emit tcx (ty_err : infer_err) (span : Span.t) =
  match ty_err with
  | MismatchTy (expected, ty) ->
      if expected <> Err && ty <> Err
      then tcx#emit (mismatch_ty expected ty span)
  | FnNotFound name -> tcx#emit (fn_not_found name span)
  | StructNotFound name -> tcx#emit (struct_not_found name span)
  | VarNotFound name -> tcx#emit (local_var_not_found name span)
  | MismatchArgs (expected, args) ->
      tcx#emit (mismatch_args expected args span)
  | InvalidDeref ty -> if ty <> Err then tcx#emit (invalid_deref ty span)
  | InvalidCall ty -> if ty <> Err then tcx#emit (invalid_call ty span)
  | AssocFnAsMethod name -> tcx#emit (assoc_call_as_method name span)
;;
