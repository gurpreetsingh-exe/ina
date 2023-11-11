open Ast
open Resolver
open Middle.Def_id
open Structures.Hashmap

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
    let _ = resolver#mod_table#insert id m in
    m
  in
  object (self)
    val resolver : resolver = resolver
    val modd = modd
    val parent = parent
    val modul = create_modul ()
    method modul = modul
    method visit_ty _ = ()
    method visit_fn_sig _ = ()
    method visit_expr _ = ()
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
      let res = Res (Def (def_id fn.func_id 0, Fn)) in
      resolver#define modul fn.fn_sig.name Value res;
      self#visit_fn_sig fn.fn_sig;
      match fn.body with Some body -> self#visit_block body | None -> ()

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
           | Some m ->
               let visitor = new visitor resolver m (Some modul) in
               visitor#visit_mod;
               let res = Module visitor#modul in
               resolver#define modul m.mod_name Type res
           | None -> ())
      | Unit _ -> ()

    method visit_mod = modd.items#iter self#visit_item
  end
