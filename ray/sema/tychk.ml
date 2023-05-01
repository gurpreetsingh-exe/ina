open Ast
open Infer
open Front.Fmt

let ty_unwrap (ty : ty option) = Option.value ty ~default:Unit

type ty_ctx = { func_map : (node_id, ty) Hashtbl.t }

type ty_err =
  | MismatchTy of ty * ty
  | NoReturn of func

exception TypeError of ty_err

let ty_err_emit ty_err =
  match ty_err with
  | MismatchTy (expected, ty) ->
      Printf.printf "\x1b[31;1merror\x1b[0m: expected `%s`, found `%s`\n"
        (render_ty expected) (render_ty ty)
  | NoReturn func ->
      Printf.printf
        "\x1b[31;1merror\x1b[0m: function `%s` returns nothing, but `%s` \
         expected\n"
        func.fn_sig.name
        (render_ty (ty_unwrap func.fn_sig.ret_ty))

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
          if ty = ret_ty then () else ty_err_emit (MismatchTy (ret_ty, ty))
      | None -> assert false)
    | None -> (
      match ret_ty with Unit -> () | _ -> ty_err_emit (NoReturn func)))
  | None -> ()

let tychk ty_ctx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> tychk_func ty_ctx func
    | _ -> assert false
  in
  List.iter f modd.items
