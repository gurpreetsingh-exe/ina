open Printf
open Ast
open Session
open Structures.Vec
open Structures.Hashmap
open Middle.Ctx
open Middle.Def_id
open Utils.Printer
open Utils.Panic
open Errors

let pcmp (a : 'a) (b : 'a) = Obj.magic a = Obj.magic b

module Module = struct
  type namespace =
    | Type
    | Value

  and module_kind =
    | Block
    | Def of (def_kind * def_id * string)

  and t = {
      mkind: module_kind
    ; parent: t option
    ; resolutions: resolutions
  }

  and name_resolution =
    | Res of res
    | Module of t

  and binding_key = {
      ident: string
    ; ns: namespace
    ; disambiguator: int
  }

  and resolutions = (binding_key, name_resolution) hashmap

  let res_to_def_kind : res -> string = function
    | Def (_, def_kind) -> print_def_kind def_kind
    | _ -> assert false
  ;;

  let get_def_kind binding =
    match binding with
    | Res res -> res_to_def_kind res
    | Module _ -> "module"
  ;;

  let binding_to_def_id : name_resolution -> def_id = function
    | Res (Def (id, _)) | Module { mkind = Def (_, id, _); _ } -> id
    | _ -> assert false
  ;;

  let name_binding_kind_to_enum = function Res _ -> 0L | Module _ -> 1L
  let render_ns = function Type -> "type" | Value -> "value"

  let module_res mdl =
    match mdl.mkind with
    | Def (kind, def_id, _) ->
        let res : res = Def (def_id, kind) in
        Some res
    | _ -> None
  ;;

  let binding_res binding =
    match binding with
    | Res res -> res
    | Module m -> Option.get (module_res m)
  ;;

  let rec get_root_path mdl =
    let name = function Block -> assert false | Def (_, _, name) -> name in
    match mdl.parent with
    | Some parent -> get_root_path parent @ [name mdl.mkind]
    | None -> [name mdl.mkind]
  ;;

  let print_binding_key key = sprintf "%s" key.ident

  let print_mkind = function
    | Block -> "block"
    | Def (kind, id, ident) ->
        sprintf "%s [%s] <%s>" ident (print_def_kind kind) (print_def_id id)
  ;;

  let rec print_nameres prefix printer nameres =
    match nameres with
    | Res res ->
        printer#append (green ?bold:(Some false) @@ q @@ print_res res);
        printer#append "\n"
    | Module mdl -> print_modul prefix printer mdl

  and print_resolution prefix printer res =
    render_children ?prefix:(Some prefix) printer res (fun (k, v) prefix ->
        match v with
        | Res _ ->
            printer#append @@ print_binding_key k;
            printer#append " ";
            print_nameres prefix printer v
        | _ -> print_nameres prefix printer v)

  and print_modul prefix printer mdl =
    printer#append
    @@ print_mkind mdl.mkind
    ^ " "
    ^ (match mdl.parent with
       | Some m -> green ?bold:(Some false) @@ q @@ print_mkind m.mkind
       | None -> "'no parent'")
    ^ "\n";
    print_resolution prefix printer mdl.resolutions
  ;;
end

type 'a nodemap = (node_id, 'a) hashmap

class disambiguator =
  object
    val mutable stack : int list = []
    method create = List.hd stack

    method inc =
      stack <- (match stack with dg :: rest -> [dg + 1] @ rest | [] -> [0])

    method dec =
      stack <- (match stack with dg :: rest -> [dg - 1] @ rest | [] -> [0])

    method push = stack <- [0] @ stack
    method pop = stack <- (match stack with _ :: rest -> rest | [] -> [0])
  end

class resolver tcx modd =
  let open Module in
  object (self)
    val tcx : tcx = tcx
    val modd : modd = modd
    val modules : (def_id, Module.t) hashmap = new hashmap
    val res : res nodemap = new hashmap
    val mutable current_qpath : string vec = new vec

    val binding_parent_module : (name_resolution, Module.t) hashmap =
      new hashmap

    val disambiguator = new disambiguator
    val block_map : (binding_key, name_resolution) hashmap = new hashmap

    val mutable unit_root : Module.t =
      { mkind = Block; parent = None; resolutions = new hashmap }

    method unit_root = unit_root
    method init mdl = unit_root <- mdl
    method res = res
    method modules = modules
    method tcx = tcx
    method sess = tcx#sess
    method append_segment segment = current_qpath#push segment
    method pop_segment = current_qpath#pop

    method set_path def_id =
      let path = new vec in
      path#copy current_qpath;
      assert (tcx#def_id_to_qpath#insert def_id path = None)

    method shadow mdl ident ns binding =
      let key = { ident; ns; disambiguator = 0 } in
      self#set_binding_parent_module binding mdl;
      ignore (mdl.resolutions#insert key binding)

    method define parent ident ns name_binding =
      let key = { ident; ns; disambiguator = 0 } in
      match self#try_define parent key name_binding with
      | Ok () -> ()
      | Error e -> self#sess.parse_sess.span_diagnostic#emit_diagnostic e

    method set_binding_parent_module binding mdl =
      match binding_parent_module#insert binding mdl with
      | Some old_modul -> if not @@ pcmp old_modul mdl then assert false
      | None -> ()

    method resolutions mdl = mdl.resolutions

    method resolution mdl key =
      let res = self#resolutions mdl in
      match res#get key with Some nameres -> nameres | None -> assert false

    method try_define mdl key binding =
      self#set_binding_parent_module binding mdl;
      match mdl.resolutions#insert key binding with
      | Some old_binding ->
          if not @@ pcmp old_binding binding
          then Error (self#redefinition_error key binding old_binding)
          else Ok ()
      | None -> Ok ()

    method redefinition_error key binding old =
      let kind = get_def_kind binding in
      let def_id = binding_to_def_id binding in
      let span = tcx#spans#unsafe_get def_id.inner in
      let old_span = tcx#spans#unsafe_get (binding_to_def_id old).inner in
      tcx#emit (Diagnostic.mk_err "previous definition" old_span);
      let msg = sprintf "%s `%s` is already defined" kind key.ident in
      Diagnostic.mk_err msg span

    method get_root_mod mdl : Module.t =
      match mdl.mkind with
      | Def (kind, id, _) ->
          assert (kind = Mod);
          Option.get @@ modules#get id
      | Block ->
          (match mdl.parent with
           | Some m -> self#get_root_mod m
           | _ -> assert false)

    method resolve_ident_in_lexical_scope mdl ident ns =
      let key = { ident; ns; disambiguator = 0 } in
      dbg
        "resolve_ident_in_lexical_scope(module = %s, ident = %s, ns = %s) = "
        (print_mkind mdl.mkind)
        ident
        (match ns with Type -> "type" | Value -> "value");
      let res =
        match mdl.resolutions#get key with
        | Some r ->
            (match r with
             | Res r -> r
             | Module mdl ->
                 (* recursive function *)
                 self#resolve_ident_in_lexical_scope
                   (self#get_root_mod mdl)
                   ident
                   ns)
        | None ->
            (match mdl.parent with
             | Some modul' ->
                 (match mdl.mkind with
                  | Block ->
                      self#resolve_ident_in_lexical_scope modul' ident ns
                  | _ -> Err)
             | _ -> Err)
      in
      dbg "%s\n" (print_res res);
      res

    method resolve_path_in_modul mdl (segs : path_segment vec) ns =
      dbg
        "resolve_path_in_module(module = %s, path = %s, ns = %s)\n"
        (print_mkind mdl.mkind)
        (segs#join "::" (fun seg -> seg.ident))
        (match ns with Type -> "type" | Value -> "value");
      let segs_len = segs#len in
      let unit_in_path_report = ref false in
      let rec f i mdl : res =
        let ns = if i = segs_len - 1 then ns else Type in
        let seg = segs#get i in
        dbg
          "  #%d: resolve_segment(module = %s, segment = %s) = "
          i
          (print_mkind mdl.mkind)
          seg.ident;
        if i <> 0 && seg.ident = "unit" && not !unit_in_path_report
        then Err
        else
          let key = { ident = seg.ident; ns; disambiguator = 0 } in
          match mdl.resolutions#get key with
          | Some r ->
              (match r with
               | Res r ->
                   dbg "%s\n"
                   @@ Utils.Printer.green ?bold:(Some false) (print_res r);
                   r
               | Module mdl ->
                   dbg "%s\n"
                   @@ Utils.Printer.green
                        ?bold:(Some false)
                        (print_mkind mdl.mkind);
                   f (i + 1) mdl)
          | None ->
              dbg "%s\n" (Utils.Printer.red "err");
              Err
      in
      f 0 mdl

    method resolve_path (mdl : Module.t) (path : path) ns : res =
      let segs = path.segments in
      let segs_len = segs#len in
      let ns = Option.value ~default:Type ns in
      dbg
        "resolve_path(module = %s, path = %s, ns = %s)\n"
        (print_mkind mdl.mkind)
        (segs#join "::" (fun seg -> seg.ident))
        (match ns with Type -> "type" | Value -> "value");
      let res : res =
        match ns, segs_len, (segs#get 0).ident with
        | Value, 1, "unit" -> Err
        | Value, 1, _ ->
            self#resolve_ident_in_lexical_scope mdl (segs#get 0).ident Value
        | _, _, "unit" ->
            segs#pop_front;
            self#resolve_path_in_modul unit_root segs ns
        | _ ->
            let mdl = self#get_root_mod mdl in
            self#resolve_path_in_modul mdl segs ns
      in
      (* dbg "%s\n" (print_res res); *)
      res

    method resolve_path_extern mdl path ns =
      let res = ref @@ self#resolve_path mdl path ns in
      (match !res with Err -> assert false | _ -> ());
      !res

    method resolve_main =
      let mdl = self#get_root_mod unit_root in
      let segs = new vec in
      segs#push { ident = "main"; span = Source.Span.make 0 0 };
      self#resolve_path_in_modul mdl segs Value

    method resolve =
      self#resolve_paths unit_root modd;
      let main = self#resolve_main in
      match main with
      | Def (id, _) -> tcx#set_main id
      | _ -> print_endline "main not found"

    method resolve_paths (mdl : Module.t) (modd : modd) : unit =
      dbg "resolve_paths(module = %s)\n" (print_mkind mdl.mkind);
      let rec resolve_ty (ty : Ast.ty) =
        match ty.kind with
        | FnTy (args, ty, _) ->
            args#iter (fun ty -> resolve_ty ty);
            resolve_ty ty
        | Path path ->
            let resolved = self#resolve_path mdl path (Some Type) in
            (match resolved with Err -> assert false | _ -> ());
            (match res#insert path.path_id resolved with
             | Some _ -> assert false
             | None -> ())
        | Ref ty | Ptr ty -> resolve_ty ty
        | ImplicitSelf | Int _ | Float _ | Bool | Str | Unit -> ()
        | _ ->
            (* print_endline (Front.Ast_printer.render_ty ty); *)
            assert false
      in
      let rec visit_expr expr (mdl : Module.t) =
        match expr.expr_kind with
        | Call (path, exprs) ->
            visit_expr path mdl;
            exprs#iter (fun expr -> visit_expr expr mdl)
        | Binary (_, left, right) ->
            visit_expr left mdl;
            visit_expr right mdl
        | Path path ->
            let resolved = self#resolve_path mdl path (Some Value) in
            (match resolved with
             | Err ->
                 let err =
                   self#sess.parse_sess.span_diagnostic#span_err
                     path.span
                     {
                       msg =
                         sprintf
                           "cannot find `%s` in this scope"
                           (path.segments#join "::" (fun s -> s.ident))
                     ; style = NoStyle
                     }
                 in
                 Sess.emit_err self#sess.parse_sess err
             | _ -> ());
            (match res#insert path.path_id resolved with
             | Some _ -> assert false
             | None -> ())
        | If { cond; then_block; else_block; _ } ->
            visit_expr cond mdl;
            visit_block then_block;
            (match else_block with
             | Some expr -> visit_expr expr mdl
             | None -> ())
        | Ref expr | Deref expr -> visit_expr expr mdl
        | Block block -> visit_block block
        | Lit _ -> ()
        | StructExpr { struct_name; fields; _ } ->
            let resolved = self#resolve_path mdl struct_name (Some Type) in
            (match resolved with Err -> assert false | _ -> ());
            (match res#insert struct_name.path_id resolved with
             | Some _ -> assert false
             | None -> ());
            fields#iter (fun (_, expr) -> visit_expr expr mdl)
        | Field (expr, _) -> visit_expr expr mdl
        | Cast (expr, ty) ->
            visit_expr expr mdl;
            resolve_ty ty
        | MethodCall (expr, _, args) ->
            visit_expr expr mdl;
            args#iter (fun expr -> visit_expr expr mdl)
      and visit_block body =
        let mdl = modules#unsafe_get (def_id body.block_id 0) in
        body.block_stmts#iter (fun stmt ->
            match stmt with
            | Stmt expr | Expr expr -> visit_expr expr mdl
            | Binding
                { binding_pat; binding_expr; binding_ty; binding_id; _ } ->
                (match binding_ty with
                 | Some ty -> resolve_ty ty
                 | None -> ());
                binding_pat |> ( function
                | PatIdent name ->
                    visit_expr binding_expr mdl;
                    let res = Res (Local binding_id) in
                    self#shadow mdl name Value res )
            | Assert (expr, _) -> visit_expr expr mdl
            | Assign (expr1, expr2) ->
                visit_expr expr1 mdl;
                visit_expr expr2 mdl);
        match body.last_expr with
        | Some expr -> visit_expr expr mdl
        | None -> ()
      in
      let visit_fn (func : func) =
        func.fn_sig.args#iter (fun { ty; _ } -> resolve_ty ty);
        match func.body with Some body -> visit_block body | None -> ()
      in
      let visit_item (item : item) =
        match item with
        | Mod { resolved_mod; _ } ->
            (match resolved_mod with
             | Some modd ->
                 let id = def_id modd.mod_id 0 in
                 let mdl = modules#unsafe_get id in
                 self#resolve_paths mdl modd
             | None -> ())
        | Fn (func, _) -> visit_fn func
        | Type (Struct _) -> assert false
        | Foreign fns -> fns#iter (fun f -> visit_fn f)
        | Impl { impl_items; _ } ->
            impl_items#iter (function AssocFn fn -> visit_fn fn)
        | Unit _ -> ()
      in
      modd.items#iter visit_item
  end
