open Ast
open Front
open Ir

let rec lower_fn (fn : func) (ctx : Context.t) : Func.t =
  let { fn_sig = { name; args; ret_ty; is_variadic; _ }; body; _ } = fn in
  let ret_ty = match ret_ty with Some ty -> ty | None -> Unit in
  let fn_ty = Func.{ name; args; ret_ty; is_variadic } in
  match body with
  | Some body ->
      Def { def_ty = fn_ty; basic_blocks = lower_fn_body body ctx }
  | None -> Decl fn_ty

and lower_expr (expr : expr) (builder : Builder.t) (ctx : Context.t) :
    Inst.value =
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

and lower_fn_body body ctx =
  let blocks = ref [] in
  let entry = Basicblock.create () in
  blocks := !blocks @ [entry];
  let builder = Builder.create entry in
  let f stmt =
    match stmt with
    | Stmt expr | Expr expr -> ignore (lower_expr expr builder ctx)
    | Assign (expr1, expr2) ->
        let left = lower_expr expr1 builder ctx in
        let right = lower_expr expr2 builder ctx in
        Builder.store left right builder
    | Binding { binding_pat; binding_ty; binding_expr; _ } -> (
      match binding_pat with
      | PatIdent ident ->
          let ty = Option.get binding_ty in
          let dst = Builder.alloca ty builder in
          Context.add_local ctx ident dst;
          let src = lower_expr binding_expr builder ctx in
          Builder.store dst src builder)
  in
  List.iter f body.block_stmts;
  (match body.last_expr with
  | Some expr ->
      let ret = lower_expr expr builder ctx in
      Builder.ret ret builder
  | None -> Builder.ret_unit builder);
  !blocks

let lower_ast (ctx : Context.t) : Module.t =
  let items = ref [] in
  let f (item : item) =
    match item with
    | Fn (func, _) -> items := !items @ [lower_fn func ctx]
    | Import _ -> ()
    | _ -> assert false
  in
  List.iter f ctx.modd.items;
  { items = !items }
