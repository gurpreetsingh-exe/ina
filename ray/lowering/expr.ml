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
    | _ -> assert false)
  | Path path ->
      let ident = Fmt.render_path path in
      let ptr = Context.find_local ctx.env ident in
      Builder.load ptr builder
  | If { cond; then_block; else_block } -> (
      let cond = lower cond builder ctx in
      let then_bb = Basicblock.create () in
      let _else_bb = Basicblock.create () in
      let join_bb = Basicblock.create () in
      Builder.br cond (Label then_bb) (Label join_bb) builder;
      Context.block_append ctx then_bb;
      let _true_expr = block_lower then_block ctx in
      match else_block with
      | Some _else_block -> assert false
      | None -> (); Builder.nop builder)
  | _ -> assert false

and block_lower (block : block) (ctx : Context.t) : Inst.value =
  let bb = Option.get ctx.block in
  let builder = Builder.create bb in
  let f stmt =
    match stmt with
    | Stmt expr | Expr expr -> ignore (lower expr builder ctx)
    | Assign (expr1, expr2) ->
        let left = lower expr1 builder ctx in
        let right = lower expr2 builder ctx in
        Builder.store left right builder
    | Binding { binding_pat; binding_ty; binding_expr; _ } -> (
      match binding_pat with
      | PatIdent ident ->
          let ty = Option.get binding_ty in
          let dst = Builder.alloca ty builder in
          Context.add_local ctx ident dst;
          let src = lower binding_expr builder ctx in
          Builder.store dst src builder)
  in
  List.iter f block.block_stmts;
  match block.last_expr with
  | Some expr -> lower expr builder ctx
  | None -> Builder.nop builder
