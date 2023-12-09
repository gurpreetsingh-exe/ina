open Ast
open Resolver
open Middle.Def_id
open Structures.Hashmap
open Module

class visitor resolver modd parent =
  let create_modul () =
    let id = def_id modd.mod_id 0 in
    let m =
      {
        mkind = Def (Mod, id, modd.mod_name)
      ; parent
      ; resolutions = new hashmap
      }
    in
    let _ = resolver#modules#insert id m in
    m
  in
  object (self)
    val resolver : resolver = resolver
    val modd = modd
    val parent = parent
    val mutable mdl = create_modul ()
    val mutable curr_fn = None
    method mdl = mdl
    method visit_ty _ = ()
    method visit_fn_sig _ = ()

    method visit_expr expr =
      resolver#tcx#insert_span expr.expr_id expr.expr_span;
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
      | Lit _ | Path _ -> ()

    method visit_pat _ = ()

    method with_module modul' f =
      let tmp = mdl in
      mdl <- modul';
      f ();
      mdl <- tmp

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
      let mdl =
        { mkind = Block; parent = Some mdl; resolutions = new hashmap }
      in
      (match curr_fn with
       | Some fn ->
           fn.fn_sig.args#iter (fun { arg; arg_id; _ } ->
               let res = Res (Local arg_id) in
               resolver#shadow mdl arg Value res);
           curr_fn <- None
       | None -> ());
      let id = def_id block.block_id 0 in
      assert (resolver#modules#insert id mdl = None);
      self#with_module mdl (fun () ->
          block.block_stmts#iter self#visit_stmt;
          match block.last_expr with
          | Some expr -> self#visit_expr expr
          | None -> ())

    method visit_fn fn =
      resolver#tcx#insert_span fn.func_id fn.fn_sig.fn_span;
      let name = fn.fn_sig.name in
      let did = def_id fn.func_id 0 in
      let did =
        if fn.is_extern then resolver#tcx#decl_extern name did else did
      in
      let res = Res (Def (did, Fn)) in
      resolver#define mdl name Value res;
      self#visit_fn_sig fn.fn_sig;
      match fn.body with
      | Some body ->
          curr_fn <- Some fn;
          self#visit_block body
      | None -> ()

    method visit_impl _ = ()

    method visit_struct strukt =
      resolver#tcx#insert_span strukt.struct_id strukt.struct_span;
      let name = strukt.ident in
      let did = def_id strukt.struct_id 0 in
      let res = Res (Def (did, Struct)) in
      resolver#define mdl name Type res

    method visit_item item =
      match item with
      | Ast.Fn (fn, _) -> self#visit_fn fn
      | Type (Struct s) -> self#visit_struct s
      | Foreign fns -> fns#iter self#visit_fn
      | Impl impl -> self#visit_impl impl
      | Mod { resolved_mod; _ } ->
          (match resolved_mod with
           | Some m ->
               let visitor = new visitor resolver m (Some mdl) in
               visitor#visit_mod;
               let res = Module visitor#mdl in
               resolver#define mdl m.mod_name Type res
           | None -> ())
      | Unit _ -> ()

    method visit_mod = modd.items#iter self#visit_item
  end
