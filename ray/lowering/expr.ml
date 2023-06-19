open Ast
open Front
open Ir

let lower (expr : expr) (builder : Builder.t) (ctx : Context.t) : Inst.value
    =
  let ty = Option.get expr.expr_ty in
  match expr.expr_kind with
  | Lit lit -> (
    match lit with
    | LitInt value -> Builder.const_int ty value
    | _ -> assert false)
  | Path path ->
      let ident = Fmt.render_path path in
      let ptr = Context.find_local ctx.env ident in
      Builder.load ptr builder
  | _ -> assert false
