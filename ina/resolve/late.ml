open Ast
open Middle.Ctx
open Middle.Ty
open Middle.Def_id
open Structures.Vec
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
          let tcx = resolver#tcx in
          let res = tcx#res_map#unsafe_get path.path_id in
          (match res with
           | Def (_, Struct) ->
               let open Middle.Ctx in
               let name = (Option.get path.segments#last).ident in
               (match tcx#lookup_assoc_fn res name with
                | Some did ->
                    ignore (tcx#res_map#insert path.path_id (Def (did, Fn)))
                | None ->
                    ignore (tcx#res_map#insert path.path_id Err);
                    resolver#not_found path)
           | Ty _ -> assert false
           | Local _ -> ()
           | Def (_, _) -> ()
           | Err -> ())
      | Lit _ -> ()

    method visit_stmt stmt =
      match stmt with
      | Stmt expr | Expr expr -> self#visit_expr expr
      | Binding { binding_expr; binding_ty; _ } ->
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

    method visit_generics generics =
      generics.params#iteri
        (fun i { kind = Ident name; generic_param_id; _ } ->
          let did = def_id generic_param_id 0 in
          let ty = resolver#tcx#ty_param i name in
          resolver#tcx#create_def did ty)

    method visit_fn (fn : func) assoc =
      resolver#append_segment fn.name;
      self#visit_generics fn.fn_generics;
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
      let subst =
        Subst
          (map fn.fn_generics.params (fun { generic_param_id; _ } ->
               let did = def_id generic_param_id 0 in
               Ty (resolver#tcx#get_def did)))
      in
      let def_id = def_id fn.func_id 0 in
      let ty =
        resolver#tcx#fn_with_sig
          def_id
          ~subst
          args
          ret
          fn.fn_sig.is_variadic
          abi
      in
      resolver#tcx#create_def def_id ty;
      if assoc then resolver#set_path def_id;
      assert (resolver#tcx#node_id_to_def_id#insert fn.func_id def_id = None);
      (match fn.body with
       | Some body ->
           curr_fn <- Some fn;
           self#visit_block body
       | None -> ());
      resolver#pop_segment

    method visit_impl impl =
      let ty = resolver#tcx#ast_ty_to_ty impl.impl_ty in
      let segments = resolver#tcx#render_ty_segments ty in
      resolver#append_segments segments;
      impl.impl_items#iter (function AssocFn fn -> self#visit_fn fn true);
      resolver#pop_segments (segments#len - 1)

    method visit_struct strukt =
      let id = strukt.struct_id in
      let def_id = def_id id 0 in
      let fields =
        map strukt.members (fun (ty, name) ->
            Field { ty = resolver#tcx#ast_ty_to_ty ty; name })
      in
      let variants = new vec in
      variants#push (Variant { def_id; fields });
      let ty = resolver#tcx#adt_with_variants def_id variants in
      assert (resolver#tcx#node_id_to_def_id#insert id def_id = None);
      resolver#tcx#create_def def_id ty

    method visit_item item =
      match item with
      | Ast.Fn (fn, _) -> self#visit_fn fn false
      | Type (Struct s) -> self#visit_struct s
      | Foreign (fns, _) -> fns#iter (fun f -> self#visit_fn f false)
      | Impl impl -> self#visit_impl impl
      | Mod { resolved_mod; _ } ->
          (match resolved_mod with
           | Some m -> (new type_lowering resolver m)#lower
           | None -> ())
      | ExternMod _ -> ()

    method visit_mod =
      resolver#append_segment modd.mod_name;
      let def_id = def_id modd.mod_id 0 in
      assert (resolver#tcx#node_id_to_def_id#insert modd.mod_id def_id = None);
      modd.items#iter self#visit_item;
      resolver#pop_segment

    method lower = self#visit_mod
  end
