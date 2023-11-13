open Ast
open Front
open Printf
open Errors
open Source
open Session
open Middle.Ctx
open Middle.Ty
open Diagnostic

type infer_ctx = {
    tcx: tcx
  ; int_ut: (ty_vid, ty option) Unification_table.t
  ; float_ut: (ty_vid, ty option) Unification_table.t
}

let infer_ctx_create tcx =
  { tcx; int_ut = { values = [||] }; float_ut = { values = [||] } }
;;

let infcx_new_int_var infcx =
  infcx.tcx#intern
    (Infer (IntVar (Unification_table.new_key infcx.int_ut None)))
;;

let infcx_new_float_var infcx =
  infcx.tcx#intern
    (Infer (FloatVar (Unification_table.new_key infcx.float_ut None)))
;;

type infer_err =
  | MismatchTy of ty * ty
  | FnNotFound of ident
  | StructNotFound of ident
  | VarNotFound of ident
  | MismatchArgs of ident * int * int
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

let mismatch_args func expected found span =
  let msg =
    sprintf
      "`%s` expected %d arg%s, found %d"
      func
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
  | MismatchArgs (func, expected, args) ->
      tcx#emit (mismatch_args func expected args span)
  | InvalidDeref ty -> if ty <> Err then tcx#emit (invalid_deref ty span)
  | InvalidCall ty -> if ty <> Err then tcx#emit (invalid_call ty span)
  | AssocFnAsMethod name -> tcx#emit (assoc_call_as_method name span)
;;
