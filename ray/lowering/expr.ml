open Ast
open Front
open Ir

let rec lower (expr : expr) (builder : Builder.t) (ctx : Context.t) :
    Inst.value =
  let ty = Option.get expr.expr_ty in
  match expr.expr_kind with
  | Binary (kind, left, right) ->
      let left = lower left builder ctx in
      let right = lower right builder ctx in
      let inst_kind = Inst.binary_kind_to_inst kind in
      Builder.add_inst_with_ty ty (Binary (inst_kind, left, right)) builder
  | Lit lit -> (
    match lit with
    | LitInt value -> Builder.const_int ty value
    | LitFloat value -> Builder.const_float ty value
    | LitStr value -> Builder.const_string ty value
    | LitBool value -> Builder.const_bool ty value)
  | Path path ->
      let ident = Fmt.render_path path in
      let ptr = Context.find_local ctx.env ident in
      Builder.load ptr builder
  | If { cond; then_block; else_block } -> (
      let cond = lower cond builder ctx in
      let then_bb = Basicblock.create () in
      let else_bb = Basicblock.create () in
      let join_bb = Basicblock.create () in
      Builder.br cond (Label then_bb) (Label else_bb) builder;
      Context.block_append ctx then_bb;
      let true_expr = lower_block then_block ctx in
      let false_expr = ref None in
      Builder.with_ctx (Builder.jmp (Label join_bb)) ctx builder;
      Context.block_append ctx else_bb;
      (match else_block with
      | Some else_block -> false_expr := Some (lower_block else_block ctx)
      | None -> ());
      Builder.with_ctx (Builder.jmp (Label join_bb)) ctx builder;
      Context.block_append ctx join_bb;
      builder.block <- Option.get ctx.block;
      match ty with
      | Unit -> Builder.nop builder
      | _ ->
          let phi =
            Builder.phi ty
              [
                (Label then_bb, true_expr);
                (Label else_bb, Option.get !false_expr);
              ]
              builder
          in
          phi)
  | Call (path, args) ->
      let fn_ty = Hashtbl.find ctx.func_map path in
      let args = List.map (fun e -> lower e builder ctx) args in
      if fn_ty.is_extern && fn_ty.abi = "intrinsic" then
        Builder.intrinsic ty fn_ty.name args builder
      else (
        let ty =
          FnTy
            ( List.map (fun (t, _) -> t) fn_ty.args,
              fn_ty.ret_ty,
              fn_ty.is_variadic )
        in
        let name =
          if fn_ty.is_extern then fn_ty.name else fn_ty.linkage_name
        in
        Builder.call ty name args builder)
  | Block block -> lower_block block ctx
  | Deref expr -> Builder.load (lower expr builder ctx) builder
  | Ref expr -> lower_lvalue expr builder ctx

and lower_lvalue (expr : expr) (_builder : Builder.t) (ctx : Context.t) :
    Inst.value =
  match expr.expr_kind with
  | Path path ->
      let ident = Fmt.render_path path in
      Context.find_local ctx.env ident
  | _ -> assert false

and lower_block (block : block) (ctx : Context.t) : Inst.value =
  let tmp = ctx.env in
  ctx.env <- { parent = Some tmp; locals = Hashtbl.create 0 };
  let bb = Option.get ctx.block in
  let builder = Builder.create bb in
  if bb.is_entry then (
    let fn = Option.get ctx.fn in
    match fn with
    | Def { def_ty = ty; _ } ->
        List.iter2
          (fun (ty, name) param ->
            let ptr = Builder.alloca ty builder in
            Context.add_local ctx name ptr;
            Builder.store param ptr builder)
          ty.args ty.params
    | _ -> ());
  let f stmt =
    match stmt with
    | Stmt expr | Expr expr -> ignore (lower expr builder ctx)
    | Assign (expr1, expr2) ->
        let left = lower_lvalue expr1 builder ctx in
        let right = lower expr2 builder ctx in
        Builder.store right left builder
    | Binding { binding_pat; binding_ty; binding_expr; _ } -> (
      match binding_pat with
      | PatIdent ident ->
          let ty = Option.get binding_ty in
          let dst = Builder.alloca ty builder in
          Context.add_local ctx ident dst;
          let src = lower binding_expr builder ctx in
          Builder.store src dst builder)
  in
  List.iter f block.block_stmts;
  let ret =
    match block.last_expr with
    | Some expr -> lower expr builder ctx
    | None -> Builder.nop builder
  in
  ctx.env <- tmp;
  ret
