open Ast
open Middle.Ctx
open Middle.Ty
open Middle.Def_id
open Structures.Vec
open Utils.Panic
open Resolver

class type_lowering resolver modd =
  object (self)
    val modd = modd
    val resolver : resolver = resolver
    val mutable curr_fn = None
    method visit_ty _ = ()

    method visit_expr expr =
      match expr.expr_kind with
      | Binary (_, left, right) ->
          self#visit_expr left;
          self#visit_expr right
      | Block block -> self#visit_block block
      | If { cond; then_block; else_block; _ } ->
          self#visit_expr cond;
          self#visit_block then_block;
          (match else_block with Some e -> self#visit_expr e | None -> ())
      | Cast (expr, _) | Field (expr, _) | Ref expr | Deref expr ->
          self#visit_expr expr
      | StructExpr { fields; _ } ->
          fields#iter (fun (_, expr) -> self#visit_expr expr)
      | Call (expr, args) | MethodCall (expr, _, args) ->
          self#visit_expr expr;
          args#iter self#visit_expr
      | Path path ->
          let res = resolver#res#unsafe_get path.path_id in
          dbg "res_map { %d -> %s }\n" path.path_id (print_res res);
          assert (resolver#tcx#res_map#insert path.path_id res = None)
      | Lit _ -> ()

    method visit_pat _ = ()

    method visit_stmt stmt =
      match stmt with
      | Stmt expr | Expr expr -> self#visit_expr expr
      | Binding { binding_pat; binding_expr; binding_ty; _ } ->
          self#visit_pat binding_pat;
          self#visit_expr binding_expr;
          (match binding_ty with Some ty -> self#visit_ty ty | None -> ())
      | Assert (cond, _) -> self#visit_expr cond
      | Assign (left, right) ->
          self#visit_expr left;
          self#visit_expr right

    method visit_block block =
      block.block_stmts#iter self#visit_stmt;
      match block.last_expr with
      | Some expr -> self#visit_expr expr
      | None -> ()

    method visit_fn fn =
      let abi : abi =
        match fn.abi with
        | "intrinsic" -> Intrinsic
        | "C" -> C
        | "default" -> Default
        | _ -> assert false
      in
      let args =
        map fn.fn_sig.args (fun { ty; _ } -> resolver#tcx#ast_ty_to_ty ty)
      in
      let ret =
        match fn.fn_sig.ret_ty with
        | Some ty -> resolver#tcx#ast_ty_to_ty ty
        | None -> resolver#tcx#types.unit
      in
      let ty =
        resolver#tcx#intern (FnPtr { args; ret; is_variadic = false; abi })
      in
      assert (resolver#tcx#node_id_to_ty#insert fn.func_id ty = None);
      match fn.body with
      | Some body ->
          curr_fn <- Some fn;
          self#visit_block body
      | None -> ()

    method visit_impl _ = ()
    method visit_type _ = ()

    method visit_item item =
      match item with
      | Ast.Fn (fn, _) -> self#visit_fn fn
      | Type ty -> self#visit_type ty
      | Foreign fns -> fns#iter self#visit_fn
      | Impl impl -> self#visit_impl impl
      | Mod { resolved_mod; _ } ->
          (match resolved_mod with
           | Some m -> (new type_lowering resolver m)#lower
           | None -> ())
      | Unit _ -> ()

    method visit_mod = modd.items#iter self#visit_item
    method lower = self#visit_mod
  end
