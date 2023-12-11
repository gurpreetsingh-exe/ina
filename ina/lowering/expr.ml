open Ast
open Context
open Structures.Vec

let rec lower_block (lcx : lcx) block =
  let tcx = lcx#tcx in
  let rec lower_block' () =
    let f stmt =
      match stmt with
      | Binding binding ->
          let { binding_expr; binding_id; _ } = binding in
          let ty = tcx#node_id_to_ty#unsafe_get binding_id in
          let ptr = lcx#bx#alloca ty in
          assert (lcx#locals#insert binding_id ptr = None);
          let src = lower binding_expr in
          lcx#bx#store src ptr
      | Assign (left, right) ->
          let dst = lower_lvalue left in
          let src = lower right in
          lcx#bx#store src dst
      | Stmt expr | Expr expr -> ignore (lower expr)
      | _ -> ()
    in
    block.block_stmts#iter f;
    match block.last_expr with Some expr -> lower expr | None -> lcx#bx#nop
  and lower_lit lit ty =
    match lit with
    | LitInt value -> lcx#bx#const_int ty value
    | LitFloat value -> lcx#bx#const_float ty value
    | LitStr value -> lcx#bx#const_string ty value
    | LitBool value -> lcx#bx#const_bool ty value
  and lower_field expr ident =
    let ptr = lower_lvalue expr in
    let ty = Ir.Inst.get_ty tcx ptr in
    let ty = Option.get @@ tcx#inner_ty ty in
    lcx#bx#gep ty ptr ident
  and lower_lvalue expr =
    match expr.expr_kind with
    | Path path ->
        let res = tcx#res_map#unsafe_get path.path_id in
        (match res with
         | Local id -> lcx#locals#unsafe_get id
         | Def (id, _) -> Global id
         | _ -> assert false)
    | Deref expr -> lower expr
    | Field (expr, ident) -> lower_field expr ident
    | _ -> assert false
  and lower expr =
    let ty = tcx#node_id_to_ty#unsafe_get expr.expr_id in
    match expr.expr_kind with
    | Binary (kind, left, right) ->
        let lazy_eval value =
          let open Ir in
          let open Inst in
          let left = lower left in
          let bb = lcx#bx#block in
          let right_bb = Basicblock.create () in
          let join_bb = Basicblock.create () in
          let join_label = Label join_bb in
          let right_label = Label right_bb in
          if value
          then lcx#bx#br left join_label right_label
          else lcx#bx#br left right_label join_label;
          lcx#append_block_with_builder right_bb;
          let right = lower right in
          let right_bb = lcx#bx#block in
          lcx#bx#jmp join_label;
          lcx#append_block_with_builder join_bb;
          let phi =
            lcx#bx#phi
              ty
              [
                Label bb, lcx#bx#const_bool tcx#types.bool value
              ; Label right_bb, right
              ]
          in
          phi
        in
        let inst_kind = Ir.Inst.binary_kind_to_inst kind in
        (match inst_kind, tcx#types.bool = ty with
         | And, true -> lazy_eval false
         | Or, true -> lazy_eval true
         | _ ->
             let left = lower left in
             let right = lower right in
             lcx#bx#binary inst_kind left right)
    | Lit lit -> lower_lit lit ty
    | Path path ->
        let res = tcx#res_map#unsafe_get path.path_id in
        (match res with
         | Local id ->
             let ptr = lcx#locals#unsafe_get id in
             lcx#bx#load ptr
         | Def (id, _) -> Global id
         | _ -> assert false)
    | Call (expr, args) ->
        let fn = lower expr in
        let ty = Ir.Inst.get_ty tcx fn in
        let args = map args (fun arg -> lower arg) in
        lcx#bx#call ty fn args
    | Deref expr ->
        let ptr = lower expr in
        lcx#bx#load ptr
    | Ref expr -> lower_lvalue expr
    | If { cond; then_block; else_block; _ } ->
        let open Ir in
        let open Inst in
        let cond = lower cond in
        let then_bb = Basicblock.create () in
        let last_then_bb = ref then_bb in
        let else_bb = Basicblock.create () in
        let last_else_bb = ref else_bb in
        let join_bb = Basicblock.create () in
        let then_label = Label then_bb in
        let else_label = Label else_bb in
        lcx#bx#br cond then_label else_label;
        lcx#append_block_with_builder then_bb;
        let true' = lower_block lcx then_block in
        last_then_bb := lcx#bx#block;
        let false' = ref None in
        lcx#bx#jmp (Label join_bb);
        lcx#append_block_with_builder else_bb;
        (match else_block with
         | Some else_block ->
             false' := Some (lower else_block);
             last_else_bb := lcx#bx#block
         | None -> ());
        lcx#bx#jmp (Label join_bb);
        lcx#append_block_with_builder join_bb;
        (match !ty with
         | Unit -> lcx#bx#nop
         | _ ->
             lcx#bx#phi
               ty
               [
                 Label !last_then_bb, true'
               ; Label !last_else_bb, Option.get !false'
               ])
    | Block block -> lower_block lcx block
    | StructExpr { fields; _ } ->
        let f = map fields (fun (_, expr) -> lower expr) in
        Const { kind = Struct f; ty }
    | Field (expr, ident) ->
        let ptr = lower_field expr ident in
        lcx#bx#load ptr
    | _ -> assert false
  in
  lower_block' ()
;;
