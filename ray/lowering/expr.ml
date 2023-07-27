open Ast
open Ty
open Front
open Ir

let load (ptr : Inst.value) builder =
  let ty = Inst.get_ty ptr in
  match ty with
  | Ptr ty | RefTy ty -> (Builder.load ptr builder, ty)
  | FnTy _ as ty -> (ptr, ty)
  | _ -> assert false

let rec lower (expr : expr) (builder : Builder.t) (ctx : Context.t) :
    Inst.value =
  let ty = Option.get expr.expr_ty in
  match expr.expr_kind with
  | Binary (kind, left, right) -> (
      let lazy_eval value =
        let right_bb = Basicblock.create () in
        let join_bb = Basicblock.create () in
        let left = lower left builder ctx in
        let bb = Option.get ctx.block in
        if value then
          Builder.br left (Label join_bb) (Label right_bb) builder
        else Builder.br left (Label right_bb) (Label join_bb) builder;
        Context.block_append ctx right_bb;
        let right_builder = Builder.create right_bb in
        let right = lower right right_builder ctx in
        let right_bb = Option.get ctx.block in
        Builder.jmp (Label join_bb) right_builder;
        Context.block_append ctx join_bb;
        builder.block <- Option.get ctx.block;
        let phi =
          Builder.phi ty
            [
              (Label bb, Builder.const_bool Bool value);
              (Label right_bb, right);
            ]
            builder
        in
        phi
      in
      match (ty, kind) with
      | Bool, And -> lazy_eval false
      | Bool, Or -> lazy_eval true
      | _ ->
          let left = lower left builder ctx in
          let right = lower right builder ctx in
          let inst_kind = Inst.binary_kind_to_inst kind in
          Builder.add_inst_with_ty ty
            (Binary (inst_kind, left, right))
            builder)
  | Lit lit -> (
    match lit with
    | LitInt value -> Builder.const_int ty value
    | LitFloat value -> Builder.const_float ty value
    | LitStr value -> Builder.const_string ty value
    | LitBool value -> Builder.const_bool ty value)
  | Path path -> (
      let ident = Fmt.render_path path in
      try
        let ptr, _ = load (Context.find_local ctx.env ident) builder in
        ptr
      with Not_found ->
        let fn_ty = Hashtbl.find ctx.func_map path in
        let name =
          if fn_ty.is_extern then fn_ty.name else fn_ty.linkage_name
        in
        Global name)
  | If { cond; then_block; else_block } -> (
      let cond = lower cond builder ctx in
      let then_bb = Basicblock.create () in
      let last_then_bb = ref then_bb in
      let else_bb = Basicblock.create () in
      let last_else_bb = ref else_bb in
      let join_bb = Basicblock.create () in
      Builder.br cond (Label then_bb) (Label else_bb) builder;
      Context.block_append ctx then_bb;
      let true_expr = lower_block then_block ctx in
      last_then_bb := Option.get ctx.block;
      let false_expr = ref None in
      Builder.with_ctx (Builder.jmp (Label join_bb)) ctx builder;
      Context.block_append ctx else_bb;
      (match else_block with
      | Some else_block ->
          Builder.with_ctx
            (fun builder ->
              false_expr := Some (lower else_block builder ctx);
              last_else_bb := Option.get ctx.block)
            ctx builder
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
                (Label !last_then_bb, true_expr);
                (Label !last_else_bb, Option.get !false_expr);
              ]
              builder
          in
          phi)
  | Call (path, args) -> (
      let ident = Fmt.render_path path in
      try
        let ptr, ty = load (Context.find_local ctx.env ident) builder in
        let args = List.map (fun e -> lower e builder ctx) args in
        Builder.call ty ptr args builder
      with Not_found ->
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
          Builder.call ty (Global name) args builder))
  | Block block -> lower_block block ctx
  | Deref expr -> Builder.load (lower expr builder ctx) builder
  | Ref expr -> lower_lvalue expr builder ctx
  | StructExpr { fields; _ } ->
      let dst = Builder.alloca ty builder in
      List.iteri
        (fun i (_, expr) ->
          let src = lower expr builder ctx in
          let ptr = Builder.gep ty dst i builder in
          Builder.store src ptr builder)
        fields;
      dst
  | Field (expr, name) ->
      let ptr = lower_lvalue expr builder ctx in
      let sty = Inst.get_ty ptr in
      let index, ty =
        match sty with
        | Ptr (Struct (_, tys) as ty) | RefTy (Struct (_, tys) as ty) ->
            let index = ref (-1) in
            List.iteri
              (fun i (field, _) -> if name = field then index := i)
              tys;
            (!index, ty)
        | _ -> assert false
      in
      let ptr = Builder.gep ty ptr index builder in
      Builder.load ptr builder
  | Cast (expr, dst_ty) -> (
      let value = lower expr builder ctx in
      let src_ty = Option.get expr.expr_ty in
      match (src_ty, dst_ty) with
      | RefTy _, Ptr _ | Ptr _, FnTy _ | FnTy _, Ptr _ | Ptr _, Ptr _ ->
          value
      | Int i1, Int i2 when size_of_int i1 = size_of_int i2 -> value
      | Ptr _, Int _ -> Builder.ptrtoint value dst_ty builder
      | Int _, Ptr _ -> Builder.inttoptr value dst_ty builder
      | _ -> assert false)

and lower_lvalue (expr : expr) (builder : Builder.t) (ctx : Context.t) :
    Inst.value =
  match expr.expr_kind with
  | Path path ->
      let ident = Fmt.render_path path in
      Context.find_local ctx.env ident
  | Field (expr, name) ->
      let ptr = lower_lvalue expr builder ctx in
      let sty = Inst.get_ty ptr in
      let index, ty =
        match sty with
        | Ptr (Struct (_, tys) as ty) ->
            let index = ref (-1) in
            List.iteri
              (fun i (field, _) -> if name = field then index := i)
              tys;
            (!index, ty)
        | _ -> assert false
      in
      Builder.gep ty ptr index builder
  | Deref expr -> lower expr builder ctx
  | _ ->
      Printf.printf "%s\n" (Fmt.render_expr expr 0);
      assert false

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
            match ty with
            | FnTy _ -> Context.add_local ctx name param
            | _ ->
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
      | PatIdent ident -> (
          let ty = Option.get binding_ty in
          match ty with
          | Struct _ ->
              let ptr = lower binding_expr builder ctx in
              Context.add_local ctx ident ptr
          | _ ->
              let dst = Builder.alloca ty builder in
              let src = lower binding_expr builder ctx in
              Context.add_local ctx ident dst;
              Builder.store src dst builder))
    | Assert (expr, string) ->
        let cond = lower expr builder ctx in
        let true_bb = Basicblock.create () in
        let false_bb = Basicblock.create () in
        let join_bb = Basicblock.create () in
        Builder.br cond (Label true_bb) (Label false_bb) builder;
        Context.block_append ctx true_bb;
        let true_builder = Builder.create true_bb in
        Builder.jmp (Label join_bb) true_builder;
        Context.block_append ctx false_bb;
        let false_builder = Builder.create false_bb in
        let loc = Token.display_span expr.expr_span in
        let msg =
          match string with
          | Some msg -> (
            match msg.expr_kind with
            | Lit (LitStr msg) ->
                "  panic at 'assertion failed: `" ^ msg ^ "`', "
            | _ -> assert false)
          | None -> "  panic at 'assertion failed', "
        in
        let msg = msg ^ loc ^ "\n" in
        Builder.trap (Builder.const_string Str msg) false_builder;
        Context.block_append ctx join_bb;
        builder.block <- Option.get ctx.block
  in
  List.iter f block.block_stmts;
  let ret =
    match block.last_expr with
    | Some expr -> lower expr builder ctx
    | None -> Builder.nop builder
  in
  ctx.env <- tmp;
  ret
