open Printf
open Ast
open Session
open Structures.Vec
open Structures.Hashmap
open Middle.Ctx
open Middle.Ty
open Middle.Def_id
open Utils.Printer
open Utils.Panic
open Errors

let pcmp (a : 'a) (b : 'a) = Obj.magic a = Obj.magic b

type namespace =
  | Type
  | Value

and module_kind =
  | Block
  | Def of (def_kind * def_id * string)

and modul = {
    mkind: module_kind
  ; parent: modul option
  ; resolutions: resolutions
}

and name_resolution =
  | Res of res
  | Module of modul

and binding_key = {
    ident: string
  ; ns: namespace
  ; disambiguator: int
}

and resolutions = (binding_key, name_resolution) hashmap

type 'a nodemap = (node_id, 'a) hashmap

let res_to_def_kind : res -> string = function
  | Def (_, def_kind) -> print_def_kind def_kind
  | _ -> assert false
;;

let get_def_kind binding =
  match binding with Res res -> res_to_def_kind res | Module _ -> "module"
;;

let binding_to_def_id : name_resolution -> def_id = function
  | Res (Def (id, _)) | Module { mkind = Def (_, id, _); _ } -> id
  | _ -> assert false
;;

let name_binding_kind_to_enum = function Res _ -> 0L | Module _ -> 1L
let render_ns = function Type -> "type" | Value -> "value"

let print_prim_ty : prim_ty -> string = function
  | Int _ -> "int"
  | Float _ -> "float"
  | Bool -> "bool"
  | Str -> "str"
;;

let print_res : res -> string = function
  | Def (id, kind) ->
      sprintf "(%s~%s)" (print_def_kind kind) (print_def_id id)
  | PrimTy ty -> print_prim_ty ty
  | Local id -> "local#" ^ string_of_int id
  | Err -> "err"
;;

let module_res modul =
  match modul.mkind with
  | Def (kind, def_id, _) ->
      let res : res = Def (def_id, kind) in
      Some res
  | _ -> None
;;

let binding_res binding =
  match binding with Res res -> res | Module m -> Option.get (module_res m)
;;

let rec get_root_path modul =
  let name = function Block -> assert false | Def (_, _, name) -> name in
  match modul.parent with
  | Some parent -> get_root_path parent @ [name modul.mkind]
  | None -> [name modul.mkind]
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
  | Module modul -> print_modul prefix printer modul

and print_resolution prefix printer res =
  render_children ?prefix:(Some prefix) printer res (fun (k, v) prefix ->
      match v with
      | Res _ ->
          printer#append @@ print_binding_key k;
          printer#append " ";
          print_nameres prefix printer v
      | _ -> print_nameres prefix printer v)

and print_modul prefix printer modul =
  printer#append
  @@ print_mkind modul.mkind
  ^ " "
  ^ (match modul.parent with
     | Some m -> green ?bold:(Some false) @@ q @@ print_mkind m.mkind
     | None -> "'no parent'")
  ^ "\n";
  print_resolution prefix printer modul.resolutions
;;

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
  object (self)
    val tcx : tcx = tcx
    val modd : modd = modd
    val mod_table : (def_id, modul) hashmap = new hashmap
    val res : res nodemap = new hashmap
    method res = res
    method mod_table = mod_table
    method tcx = tcx
    method sess = tcx#sess

    val binding_parent_module : (name_resolution, modul) hashmap =
      new hashmap

    val disambiguator = new disambiguator
    val block_map : (binding_key, name_resolution) hashmap = new hashmap

    val mutable unit_root : modul =
      { mkind = Block; parent = None; resolutions = new hashmap }

    method unit_root = unit_root
    method init modul = unit_root <- modul

    method shadow modul ident ns binding =
      let key = { ident; ns; disambiguator = 0 } in
      self#set_binding_parent_module binding modul;
      ignore (modul.resolutions#insert key binding)

    method define parent ident ns name_binding =
      let key = { ident; ns; disambiguator = 0 } in
      match self#try_define parent key name_binding with
      | Ok () -> ()
      | Error e -> self#sess.parse_sess.span_diagnostic#emit_diagnostic e

    method set_binding_parent_module binding modul =
      match binding_parent_module#insert binding modul with
      | Some old_modul -> if not @@ pcmp old_modul modul then assert false
      | None -> ()

    method resolutions modul = modul.resolutions

    method resolution modul key =
      let res = self#resolutions modul in
      match res#get key with Some nameres -> nameres | None -> assert false

    method try_define modul key binding =
      self#set_binding_parent_module binding modul;
      match modul.resolutions#insert key binding with
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

    method get_root_mod modul : modul =
      match modul.mkind with
      | Def (kind, id, _) ->
          assert (kind = Mod);
          Option.get @@ mod_table#get id
      | Block ->
          (match modul.parent with
           | Some m -> self#get_root_mod m
           | _ -> assert false)

    method resolve_ident_in_lexical_scope modul ident ns =
      let key = { ident; ns; disambiguator = 0 } in
      dbg
        "resolve_ident_in_lexical_scope(module = %s, ident = %s, ns = %s)\n"
        (print_mkind modul.mkind)
        ident
        (match ns with Type -> "type" | Value -> "value");
      match modul.resolutions#get key with
      | Some r ->
          (match r with
           | Res r -> r
           | Module modul ->
               (* recursive function *)
               self#resolve_ident_in_lexical_scope
                 (self#get_root_mod modul)
                 ident
                 ns)
      | None ->
          (match modul.parent with
           | Some modul' ->
               (match modul.mkind with
                | Block ->
                    self#resolve_ident_in_lexical_scope modul' ident ns
                | _ -> Err)
           | _ -> Err)

    method resolve_path_in_modul modul (segs : path_segment vec) ns =
      let segs_len = segs#len in
      let unit_in_path_report = ref false in
      let rec f i modul : res =
        let ns = if i = segs_len - 1 then ns else Type in
        let seg = segs#get i in
        if i <> 0 && seg.ident = "unit" && not !unit_in_path_report
        then Err
        else
          let key = { ident = seg.ident; ns; disambiguator = 0 } in
          match modul.resolutions#get key with
          | Some r ->
              (match r with Res r -> r | Module modul -> f (i + 1) modul)
          | None -> Err
      in
      f 0 modul

    method resolve_path (modul : modul) (path : path) ns : res =
      let segs = path.segments in
      let segs_len = segs#len in
      let ns = Option.value ~default:Type ns in
      dbg
        "resolve_path(module = %s, path = %s, ns = %s)\n"
        (print_mkind modul.mkind)
        (segs#join "::" (fun seg -> seg.ident))
        (match ns with Type -> "type" | Value -> "value");
      match ns, segs_len, (segs#get 0).ident with
      | Value, 1, "unit" -> Err
      | Value, 1, _ ->
          self#resolve_ident_in_lexical_scope modul (segs#get 0).ident Value
      | _, _, "unit" ->
          segs#pop_front;
          self#resolve_path_in_modul unit_root segs ns
      | _ ->
          let modul = self#get_root_mod modul in
          self#resolve_path_in_modul modul segs ns

    method resolve_path_extern modul path ns =
      let res = ref @@ self#resolve_path modul path ns in
      (match !res with Err -> assert false | _ -> ());
      !res

    method resolve =
      dbg "resolve()\n";
      self#resolve_paths unit_root modd

    method resolve_paths (modul : modul) (modd : modd) : unit =
      dbg "resolve_paths(module = %s)\n" (print_mkind modul.mkind);
      let rec resolve_ty (ty : Ast.ty) =
        match ty.kind with
        | FnTy (args, ty, _) ->
            args#iter (fun ty -> resolve_ty ty);
            resolve_ty ty
        | Path path ->
            let resolved = self#resolve_path modul path (Some Type) in
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
      let rec visit_expr expr (modul : modul) =
        match expr.expr_kind with
        | Call (path, exprs) ->
            visit_expr path modul;
            exprs#iter (fun expr -> visit_expr expr modul)
        | Binary (_, left, right) ->
            visit_expr left modul;
            visit_expr right modul
        | Path path ->
            let resolved = self#resolve_path modul path (Some Value) in
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
            print_endline @@ print_res resolved;
            (match res#insert path.path_id resolved with
             | Some _ -> assert false
             | None -> ())
        | If { cond; then_block; else_block; _ } ->
            visit_expr cond modul;
            visit_block then_block;
            (match else_block with
             | Some expr -> visit_expr expr modul
             | None -> ())
        | Ref expr | Deref expr -> visit_expr expr modul
        | Block block -> visit_block block
        | Lit _ -> ()
        | StructExpr { struct_name; fields; _ } ->
            let resolved = self#resolve_path modul struct_name (Some Type) in
            (match resolved with Err -> assert false | _ -> ());
            (match res#insert struct_name.path_id resolved with
             | Some _ -> assert false
             | None -> ());
            fields#iter (fun (_, expr) -> visit_expr expr modul)
        | Field (expr, _) -> visit_expr expr modul
        | Cast (expr, ty) ->
            visit_expr expr modul;
            resolve_ty ty
        | MethodCall (expr, _, args) ->
            visit_expr expr modul;
            args#iter (fun expr -> visit_expr expr modul)
      and visit_block body =
        let modul = mod_table#unsafe_get (def_id body.block_id 0) in
        body.block_stmts#iter (fun stmt ->
            match stmt with
            | Stmt expr | Expr expr -> visit_expr expr modul
            | Binding { binding_pat; binding_expr = expr; binding_ty; _ } ->
                (match binding_ty with
                 | Some ty -> resolve_ty ty
                 | None -> ());
                binding_pat |> ( function
                | PatIdent _ -> visit_expr expr modul )
            | Assert (expr, _) -> visit_expr expr modul
            | Assign (expr1, expr2) ->
                visit_expr expr1 modul;
                visit_expr expr2 modul);
        match body.last_expr with
        | Some expr -> visit_expr expr modul
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
                 let modul = mod_table#unsafe_get id in
                 self#resolve_paths modul modd
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
