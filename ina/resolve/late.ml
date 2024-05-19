open Ast
open Middle.Ctx
open Middle.Ty
open Middle.Def_id
open Structures.Vec
open Structures.Hashmap
open Resolver

class type_lowering resolver modd =
  object (self)
    val modd : Ast.modd = modd
    val resolver : resolver = resolver
    val mutable curr_fn = None
    val mutable parent = None
    method visit_ty _ = ()

    method with_parent did f =
      parent <- Some did;
      f ();
      parent <- None

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
      | Cast (expr, _) | Field (expr, _) | Ref (_, expr) | Deref expr ->
          self#visit_expr expr
      | StructExpr { fields; _ } ->
          fields#iter (fun (_, expr) -> self#visit_expr expr)
      | Call (expr, args) | MethodCall (expr, _, args) ->
          self#visit_expr expr;
          args#iter self#visit_expr
      | Path path -> self#visit_path path
      | Lit _ -> ()
      | Match (expr, arms) ->
          self#visit_expr expr;
          arms#iter (fun { pat; expr; _ } ->
              self#visit_pat pat;
              self#visit_expr expr)
      | Slice exprs -> exprs#iter self#visit_expr
      | Index (expr, idx) ->
          self#visit_expr expr;
          self#visit_expr idx

    method visit_path path =
      let tcx = resolver#tcx in
      let res = tcx#res_map#unsafe_get path.path_id in
      match res with
      | Def (id, ((Struct | Adt) as kind)) ->
          let open Middle.Ctx in
          let name = (Option.get path.segments#last).ident in
          (match tcx#lookup_assoc_fn res name with
           | Some did ->
               (if path.segments#len >= 2
                then
                  let slast = path.segments#get (-2) in
                  ignore (tcx#res_map#insert slast.id (Def (id, kind))));
               ignore (tcx#res_map#insert path.path_id (Def (did, AssocFn)))
           | None ->
               ignore (tcx#res_map#insert path.path_id Err);
               resolver#not_found path)
      | Ty _ -> assert false
      | Local _ -> ()
      | Def (_, _) -> ()
      | Err -> ()

    method visit_pat pat =
      match pat with
      | PPath path -> self#visit_path path
      | PCons (path, pats) ->
          self#visit_path path;
          pats#iter (fun pat -> self#visit_pat pat)
      | _ -> ()

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
      let parent_count =
        Option.fold
          ~none:0
          ~some:(fun did ->
            let generics = resolver#tcx#generics_of did in
            Generics.count generics)
          parent
      in
      let param_def_id_to_index = new hashmap in
      let params =
        mapi generics.params (fun i param ->
            match param.kind with
            | Ident name ->
                let def_id = local_def_id param.id in
                let index = parent_count + i in
                assert (param_def_id_to_index#insert def_id index = None);
                let ty = resolver#tcx#ty_param index name in
                resolver#tcx#create_def def_id ty;
                Generics.{ name; def_id; index })
      in
      let generics' = Generics.{ parent; parent_count; params } in
      generics', Subst (Generics.to_subst generics' resolver#tcx)

    method visit_fn (fn : func) =
      let generics, subst = self#visit_generics fn.fn_generics in
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
      let def_id = local_def_id fn.func_id in
      resolver#tcx#define_generics def_id generics;
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
      assert (resolver#tcx#node_id_to_def_id#insert fn.func_id def_id = None);
      match fn.body with
      | Some body ->
          curr_fn <- Some fn;
          self#visit_block body
      | None -> ()

    method visit_impl impl =
      let generics, _ = self#visit_generics impl.generics in
      let did = local_def_id impl.id in
      resolver#tcx#define_generics did generics;
      self#with_parent did (fun () ->
          resolver#tcx#def_key did |> function
          | { data = Impl id; _ } ->
              let ty = resolver#tcx#ast_ty_to_ty impl.ty in
              resolver#tcx#link_impl id ty;
              impl.items#iter (function AssocFn fn -> self#visit_fn fn)
          | _ -> assert false)

    method visit_struct (strukt : strukt) =
      let generics, subst = self#visit_generics strukt.generics in
      let id = strukt.id in
      let def_id = local_def_id id in
      resolver#tcx#define_generics def_id generics;
      let fields =
        map strukt.fields (fun (ty, name) ->
            Field { ty = resolver#tcx#ast_ty_to_ty ty; name })
      in
      let variants = new vec in
      variants#push (Variant { def_id; fields; index = 0 });
      let ty =
        resolver#tcx#adt_with_variants def_id variants StructT subst
      in
      assert (resolver#tcx#node_id_to_def_id#insert id def_id = None);
      resolver#tcx#create_def def_id ty

    method visit_item item =
      match item with
      | Ast.Fn (fn, _) -> self#visit_fn fn
      | Type (Struct s) -> self#visit_struct s
      | Type (Adt adt) -> self#visit_adt adt
      | Foreign (fns, _) -> fns#iter (fun f -> self#visit_fn f)
      | Impl impl -> self#visit_impl impl
      | Mod { resolved_mod; _ } ->
          (match resolved_mod with
           | Some m -> (new type_lowering resolver m)#lower
           | None -> ())
      | ExternMod _ | Using _ -> ()

    method visit_adt adt =
      let generics, subst = self#visit_generics adt.generics in
      let id = adt.id in
      let def_id = local_def_id id in
      resolver#tcx#define_generics def_id generics;
      let variants =
        mapi adt.variants (fun i variant ->
            let def_id = local_def_id variant.id in
            let fields =
              mapi variant.fields (fun i ty ->
                  Field
                    {
                      ty = resolver#tcx#ast_ty_to_ty ty
                    ; name = string_of_int i
                    })
            in
            resolver#tcx#variant def_id fields i)
      in
      let ty = resolver#tcx#adt_with_variants def_id variants AdtT subst in
      assert (resolver#tcx#node_id_to_def_id#insert id def_id = None);
      resolver#tcx#create_def def_id ty

    method visit_mod =
      let def_id = local_def_id modd.mod_id in
      assert (resolver#tcx#node_id_to_def_id#insert modd.mod_id def_id = None);
      modd.items#iter self#visit_item

    method lower = self#visit_mod
  end
