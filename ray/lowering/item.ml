open Ast
open Ir

let rec lower_fn (fn : func) (ctx : Context.t) : Func.t =
  let { fn_sig = { name; args; ret_ty; is_variadic; _ }; body; _ } = fn in
  let ret_ty = match ret_ty with Some ty -> ty | None -> Unit in
  let fn_ty = Func.{ name; args; ret_ty; is_variadic } in
  match body with
  | Some body ->
      Def { def_ty = fn_ty; basic_blocks = lower_fn_body body ctx }
  | None -> Decl fn_ty

and lower_fn_body body ctx =
  let blocks = ref [] in
  let entry = Basicblock.create () in
  blocks := !blocks @ [entry];
  let builder = Builder.create entry in
  let f stmt =
    match stmt with
    | Stmt expr | Expr expr -> ignore (Expr.lower expr builder ctx)
    | Assign (expr1, expr2) ->
        let left = Expr.lower expr1 builder ctx in
        let right = Expr.lower expr2 builder ctx in
        Builder.store left right builder
    | Binding { binding_pat; binding_ty; binding_expr; _ } -> (
      match binding_pat with
      | PatIdent ident ->
          let ty = Option.get binding_ty in
          let dst = Builder.alloca ty builder in
          Context.add_local ctx ident dst;
          let src = Expr.lower binding_expr builder ctx in
          Builder.store dst src builder)
  in
  List.iter f body.block_stmts;
  (match body.last_expr with
  | Some expr ->
      let ret = Expr.lower expr builder ctx in
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