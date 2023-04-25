open Ast
open Front.Fmt

type ty_ctx = { func_map : (node_id, ty) Hashtbl.t }

type ty_err = MismatchTy of ty * ty

exception TypeError of ty_err

let ty_err_emit ty_err =
  match ty_err with
  | MismatchTy (expected, ty) ->
      Printf.printf "expected `%s`, found `%s`\n" (render_ty expected)
        (render_ty ty)

let ty_ctx_create () = { func_map = Hashtbl.create 0 }

let infer _ty_ctx expr : ty =
  match expr with Lit (LitInt _) -> Prim I64 | Lit (LitBool _) -> Prim Bool

let infer_block ty_ctx block : ty =
  let f stmt : ty =
    match stmt with Stmt expr -> infer ty_ctx expr | Binding _ -> Unit
  in
  ignore (List.map f block.block_stmts);
  match block.last_expr with Some expr -> infer ty_ctx expr | None -> Unit

let tychk_func ty_ctx (func : func) =
  let { fn_sig; body; func_id; _ } = func in
  let { args; ret_ty; _ } = fn_sig in
  let ret_ty = Option.value ret_ty ~default:Unit in
  Hashtbl.add ty_ctx.func_map func_id
    (FnTy (List.map (fun (ty, _) -> ty) args, ret_ty));
  match body with
  | Some body ->
      let ty = infer_block ty_ctx body in
      if ret_ty = ty then () else ty_err_emit (MismatchTy (ret_ty, ty))
  | None -> ()

let tychk ty_ctx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> tychk_func ty_ctx func
    | _ -> assert false
  in
  List.iter f modd.items
