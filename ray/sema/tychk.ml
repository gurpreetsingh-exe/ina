open Ast
open Infer
open Front.Fmt

type ty_ctx = { func_map : (node_id, ty) Hashtbl.t }

type ty_err = MismatchTy of ty * ty

exception TypeError of ty_err

let ty_err_emit ty_err =
  match ty_err with
  | MismatchTy (expected, ty) ->
      Printf.printf "\x1b[31;1merror\x1b[0m: expected `%s`, found `%s`\n"
        (render_ty expected) (render_ty ty)

let ty_ctx_create () = { func_map = Hashtbl.create 0 }

let ty_ctx_create (infer_ctx : infer_ctx) = { func_map = infer_ctx.func_map }

let tychk_func _ty_ctx (func : func) =
  let { fn_sig = { ret_ty; _ }; body; _ } = func in
  let ret_ty = Option.value ret_ty ~default:Unit in
  match body with
  | Some body -> (
    match body.last_expr with
    | Some expr -> (
      match expr.expr_ty with
      | Some ty ->
          if ty = ret_ty then () else ty_err_emit (MismatchTy (ty, ret_ty))
      | None -> ty_err_emit (MismatchTy (ret_ty, Unit)))
    | None -> ())
  | None -> ()

let tychk ty_ctx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> tychk_func ty_ctx func
    | _ -> assert false
  in
  List.iter f modd.items
