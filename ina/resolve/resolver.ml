open Printf
open Ast
open Structures.Vec
open Structures.Hashmap
open Middle.Ctx
open Middle.Def_id
open Utils.Printer
open Utils.Panic
open Errors
open Diagnostic

let pcmp (a : 'a) (b : 'a) = Obj.magic a = Obj.magic b

module Module = struct
  type namespace =
    | Type
    | Value

  and module_kind =
    | Block
    | Def of (def_kind * def_id * string)

  and import_kind =
    | Named of {
          source: ident
        ; ns: namespace option
      }
    | Glob

  and import = {
      ikind: import_kind
    ; mutable res: res
  }

  and t = {
      mkind: module_kind
    ; parent: t option
    ; resolutions: resolutions
    ; imports: import vec
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

  let binding_to_def_id : name_resolution -> def_id = function
    | Res (Def (id, _)) | Module { mkind = Def (_, id, _); _ } -> id
    | _ -> assert false
  ;;

  let binding_to_def_kind : name_resolution -> def_kind = function
    | Res (Def (_, kind)) | Module { mkind = Def (kind, _, _); _ } -> kind
    | _ -> assert false
  ;;

  let binding_to_def_path name : name_resolution -> def_data = function
    | Res (Def (_, (Fn | Intrinsic))) -> ValueNs name
    | Res (Def (_, (Mod | Struct))) | Module { mkind = Def (Mod, _, _); _ }
      ->
        TypeNs name
    | _ -> assert false
  ;;

  let encode_import enc import =
    (match import.ikind with
     | Named { source; ns } ->
         enc#emit_with 0L (fun e ->
             e#emit_str source;
             match ns with
             | Some ns ->
                 e#emit_with 0L (fun e ->
                     let n = match ns with Type -> 0 | Value -> 1 in
                     e#emit_usize n)
             | None -> e#emit_usize 1)
     | Glob -> enc#emit_usize 1);
    encode_res enc import.res
  ;;

  let decode_import dec =
    let ikind =
      match dec#read_usize with
      | 0 ->
          let source = dec#read_str in
          let ns =
            match dec#read_usize with
            | 0 ->
                Some
                  (match dec#read_usize with
                   | 0 -> Type
                   | 1 -> Value
                   | _ -> assert false)
            | 1 -> None
            | _ -> assert false
          in
          Named { source; ns }
      | 1 -> Glob
      | _ -> assert false
    in
    let res = decode_res dec in
    { ikind; res }
  ;;

  let rec encode enc mdl =
    (match mdl.mkind with
     | Def (kind, id, name) ->
         enc#emit_with 0L (fun e ->
             e#emit_usize (Middle.Def_id.def_kind_to_enum kind);
             Middle.Def_id.encode e id;
             e#emit_str name)
     | Block -> assert false);
    enc#emit_usize mdl.resolutions#len;
    mdl.resolutions#iter (fun key binding ->
        enc#emit_str key.ident;
        enc#emit_u32 key.disambiguator;
        enc#emit_u8 (match key.ns with Type -> 0 | Value -> 1);
        match binding with
        | Res res -> enc#emit_with 0L (fun e -> encode_res e res)
        | Module m -> enc#emit_with 1L (fun e -> encode e m));
    Metadata.Encoder.encode_vec enc mdl.imports encode_import
  ;;

  let rec decode resolver dec parent =
    let mkind =
      match dec#read_usize with
      | 0 ->
          let kind =
            Middle.Def_id.def_kind_of_enum dec#read_usize |> Option.get
          in
          let def_id = Middle.Def_id.decode dec in
          let name = dec#read_str in
          Def (kind, def_id, name)
      | i ->
          printf "%i\n" i;
          assert false
    in
    let nmdl = dec#read_usize in
    let mdl =
      { mkind; parent; resolutions = new hashmap; imports = new vec }
    in
    (match mkind with
     | Def ((Mod | Adt), id, _) -> resolver#modules#insert' id mdl
     | _ -> assert false);
    for _ = 0 to nmdl - 1 do
      let ident = dec#read_str in
      let (_ : int) = dec#read_u32 in
      let ns =
        dec#read_u8 |> function 0 -> Type | 1 -> Value | _ -> assert false
      in
      let binding =
        match dec#read_usize with
        | 0 -> Res (decode_res dec)
        | 1 -> Module (decode resolver dec (Some mdl))
        | _ -> assert false
      in
      resolver#define mdl ident ns binding
    done;
    Metadata.Decoder.decode_vec dec mdl.imports decode_import;
    mdl
  ;;

  let res_to_def_kind : res -> string = function
    | Def (_, def_kind) -> print_def_kind def_kind
    | _ -> assert false
  ;;

  let get_def_kind binding =
    match binding with
    | Res res -> res_to_def_kind res
    | Module _ -> "module"
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

  let print mdl =
    let printer = new Utils.Printer.printer in
    print_modul "" printer mdl;
    printer#print
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

class scope =
  let open Module in
  object
    val bindings : (binding_key, res) hashmap = new hashmap
    method bindings = bindings

    method define ident ns res =
      assert (bindings#insert { ident; ns; disambiguator = 0 } res = None)
  end

let with_generics_params resolver generics f =
  let type_scope = new scope in
  generics.params#iter (fun { kind = Ident name; id; _ } ->
      let did = local_def_id id in
      let res = Middle.Ctx.Def (did, TyParam) in
      type_scope#define name Type res);
  resolver#scopes#push type_scope;
  f ();
  ignore resolver#scopes#pop
;;

class resolver tcx modd =
  let open Module in
  object (self)
    val tcx : tcx = tcx
    val modd : modd = modd
    val modules : (def_id, Module.t) hashmap = new hashmap
    val extmods : (string, Module.t) hashmap = new hashmap
    val imports : (node_id, import) hashmap = new hashmap
    val mutable current_impl : res option = None
    val scopes : scope vec = new vec

    val binding_parent_module : (name_resolution, Module.t) hashmap =
      new hashmap

    val disambiguator = new disambiguator
    val block_map : (binding_key, name_resolution) hashmap = new hashmap

    val mutable mod_root : Module.t =
      {
        mkind = Block
      ; parent = None
      ; resolutions = new hashmap
      ; imports = new vec
      }

    method mod_root = mod_root
    method init mdl = mod_root <- mdl
    method modules = modules
    method extmods = extmods
    method imports = imports
    method tcx = tcx
    method sess = tcx#sess
    method scopes = scopes

    method shadow mdl ident ns binding =
      let key = { ident; ns; disambiguator = 0 } in
      (* self#set_binding_parent_module binding mdl; *)
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

    method res_to_module =
      function
      | Middle.Ctx.Def (did, (Mod | Adt)) -> Some (modules#unsafe_get did)
      | _ -> None

    method try_define mdl key binding =
      (* self#set_binding_parent_module binding mdl; *)
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
      | Def (kind, _, _) ->
          dbg "get_root_mod(module = %s)\n" (Module.print_mkind mdl.mkind);
          assert (kind = Mod);
          mdl
      | Block ->
          (match mdl.parent with
           | Some m -> self#get_root_mod m
           | _ -> assert false)

    method resolve_path_in_imports mdl (segments : path_segment vec) : res =
      dbg
        "resolve_path_in_imports(module = %s, path = %s)\n%!"
        (print_mkind mdl.mkind)
        (segments#join "::" (fun seg -> seg.ident));
      let len = mdl.imports#len in
      let rec f i : res =
        if i >= len
        then Err
        else
          let import = mdl.imports#get i in
          let res =
            match import.res with
            | Err -> import.res
            | res ->
                let did = binding_to_def_id (Res res) in
                let mdl = modules#unsafe_get did in
                (match import.ikind with
                 | Named { source; ns } when source = (segments#get 0).ident
                   ->
                     (match ns with
                      | Some ns -> self#resolve_path_in_modul mdl segments ns
                      | None ->
                          let tyres =
                            self#resolve_path_in_modul mdl segments Type
                          in
                          if tyres = Err
                          then self#resolve_path_in_modul mdl segments Value
                          else tyres)
                 | Glob ->
                     let res =
                       self#resolve_path_in_modul mdl segments Type
                     in
                     if res = Err
                     then self#resolve_path_in_modul mdl segments Value
                     else res
                 | _ -> Err)
          in
          if res = Err then f (i + 1) else res
      in
      if len = 0 then Err else f 0

    method resolve_ident_in_lexical_scope mdl (segment : path_segment) ns =
      let ident = segment.ident in
      let key = { ident; ns; disambiguator = 0 } in
      dbg
        "resolve_ident_in_lexical_scope(module = %s, ident = %s, ns = %s)\n\
         %!"
        (print_mkind mdl.mkind)
        ident
        (render_ns ns);
      let res =
        match mdl.resolutions#get key with
        | Some r ->
            (match r with
             | Res r -> r
             | Module mdl ->
                 (* recursive function *)
                 self#resolve_ident_in_lexical_scope
                   (self#get_root_mod mdl)
                   segment
                   ns)
        | None ->
            let res =
              let segments = new vec in
              segments#push segment;
              self#resolve_path_in_imports mdl segments
            in
            (match mdl.parent with
             | Some modul' when res = Err ->
                 (match mdl.mkind with
                  | Block ->
                      self#resolve_ident_in_lexical_scope modul' segment ns
                  | _ -> Err)
             | _ -> res)
      in
      tcx#res_map#insert' segment.id res;
      res

    method resolve_path_in_modul mdl (segs : path_segment vec) ns : res =
      dbg
        "resolve_path_in_module(module = %s, path = %s, ns = %s)\n%!"
        (print_mkind mdl.mkind)
        (segs#join "::" (fun seg -> seg.ident))
        (render_ns ns);
      let segs_len = segs#len in
      let mod_in_path_report = ref false in
      let rec f i mdl : res =
        let ns = if i = segs_len - 1 then ns else Type in
        let seg = segs#get i in
        dbg
          "  #%d: resolve_segment(module = %s, segment = %s)\n%!"
          i
          (print_mkind mdl.mkind)
          seg.ident;
        let res =
          if i <> 0 && seg.ident = "mod" && not !mod_in_path_report
          then (Err : res)
          else
            let key = { ident = seg.ident; ns; disambiguator = 0 } in
            match mdl.resolutions#get key with
            | Some r ->
                (match r with
                 | Res r -> r
                 | Module mdl ->
                     let i = i + 1 in
                     if i = segs_len
                     then
                       Def
                         ( Module.binding_to_def_id r
                         , Module.binding_to_def_kind r )
                     else (
                       tcx#res_map#insert'
                         seg.id
                         (Def
                            ( Module.binding_to_def_id r
                            , Module.binding_to_def_kind r ));
                       f i mdl))
            | None ->
                let segments = new vec in
                segments#push seg;
                let res = self#resolve_path_in_imports mdl segments in
                let i = i + 1 in
                let res =
                  if i = segs_len
                  then res
                  else
                    match self#res_to_module res with
                    | Some mdl ->
                        tcx#res_map#insert' seg.id res;
                        f i mdl
                    | None -> res
                in
                res
        in
        tcx#res_map#insert' seg.id res;
        res
      in
      f 0 mdl

    method resolve_path (mdl : Module.t) (path : path) ns : res =
      let segs = path.segments in
      let segs_len = segs#len in
      let ns = Option.value ~default:Type ns in
      dbg
        "resolve_path(module = %s, path = %s, ns = %s)\n%!"
        (print_mkind mdl.mkind)
        (segs#join "::" (fun seg -> seg.ident))
        (render_ns ns);
      let res : res =
        match ns, segs_len, (segs#get 0).ident with
        | Value, 1, "mod" -> Err
        | Value, 1, _ ->
            self#resolve_ident_in_lexical_scope mdl (segs#get 0) Value
        | Type, 1, _ when not scopes#empty ->
            let seg = segs#get 0 in
            let ident = seg.ident in
            let key = { ident; ns; disambiguator = 0 } in
            let rec f i =
              match (scopes#get i)#bindings#get key with
              | Some res -> res
              | None when i <= 0 ->
                  let mdl = self#get_root_mod mdl in
                  self#resolve_path_in_modul mdl segs ns
              | None -> f (i - 1)
            in
            let res = f (scopes#len - 1) in
            let res =
              if res = Err
              then self#resolve_path_in_imports mdl segs
              else res
            in
            tcx#res_map#insert' seg.id res;
            res
        | _, _, "mod" ->
            ignore @@ segs#pop_front;
            if segs#empty
            then
              let did = binding_to_def_id (Module mod_root) in
              Def (did, Mod)
            else self#resolve_path_in_modul mod_root segs ns
        | _ ->
            let mdl' = self#get_root_mod mdl in
            let res = self#resolve_path_in_modul mdl' segs ns in
            if res = Err then self#resolve_path_in_imports mdl segs else res
      in
      dbg " := %s\n%!" (print_res res);
      res

    method resolve_path_extern mdl path ns =
      let res = ref @@ self#resolve_path mdl path ns in
      (match !res with Err -> assert false | _ -> ());
      !res

    method resolve_main =
      let mdl = self#get_root_mod mod_root in
      let segs = new vec in
      segs#push
        { ident = "main"; args = None; span = Source.Span.make 0 0; id = 0 };
      self#resolve_path_in_modul mdl segs Value

    method resolve =
      self#resolve_paths mod_root modd;
      if tcx#sess.options.output_type == Exe
      then
        let main = self#resolve_main in
        match main with
        | Def (id, _) -> tcx#set_main id
        | _ -> print_endline "main not found"

    method not_found path =
      let msg =
        sprintf
          "cannot find `%s` in this scope"
          (path.segments#join "::" (fun s -> s.ident))
      in
      Diagnostic.create
        msg
        ~labels:[Label.primary "not found in this scope" path.span]
      |> tcx#emit

    method not_found_in_module path name span =
      let path = path.segments#join "::" (fun s -> s.ident) in
      let msg = sprintf "cannot find `%s` in `%s`" name path in
      Diagnostic.create
        msg
        ~labels:[Label.primary (sprintf "not found in `%s`" path) span]
      |> tcx#emit

    method resolve_paths (mdl : Module.t) (modd : modd) : unit =
      dbg "resolve_paths(module = %s)\n" (print_mkind mdl.mkind);
      let rec resolve_ty (ty : Ast.ty) =
        match ty.kind with
        | FnPtr (args, ty, _) ->
            args#iter (fun ty -> resolve_ty ty);
            resolve_ty ty
        | Path path -> visit_path path mdl (Some Type)
        | Ref (_, ty) | Ptr (_, ty) | Slice ty -> resolve_ty ty
        | Int _ | Float _ | Bool | Str | Unit | CVarArgs -> ()
        | ImplicitSelf ->
            (match current_impl with
             | Some res -> assert (tcx#res_map#insert ty.ty_id res = None)
             | None -> assert false)
        | _ ->
            print_endline (Front.Ast_printer.render_ty ty);
            assert false
      and visit_segment (segment : path_segment) =
        match segment.args with
        | Some args -> args#iter resolve_ty
        | None -> ()
      and visit_path path mdl ns =
        let resolved =
          match self#resolve_path mdl path ns with
          | Err ->
              let resolved =
                Seq.fold_left
                  (fun resolved mdl ->
                    match resolved with
                    | Middle.Ctx.Err -> self#resolve_path mdl path ns
                    | _ -> resolved)
                  Err
                  extmods#values
              in
              (match resolved with Err -> self#not_found path | _ -> ());
              resolved
          | res -> res
        in
        path.segments#iter visit_segment;
        dbg "res_map { %d -> %s }\n" path.path_id (print_res resolved);
        assert (tcx#res_map#insert path.path_id resolved = None)
      and visit_expr expr (mdl : Module.t) =
        match expr.expr_kind with
        | Call (path, exprs) ->
            visit_expr path mdl;
            exprs#iter (fun expr -> visit_expr expr mdl)
        | Binary (_, left, right) ->
            visit_expr left mdl;
            visit_expr right mdl
        | Path path -> visit_path path mdl (Some Value)
        | If { cond; then_block; else_block; _ } ->
            visit_expr cond mdl;
            visit_block then_block;
            (match else_block with
             | Some expr -> visit_expr expr mdl
             | None -> ())
        | Ref (_, expr) | Deref expr -> visit_expr expr mdl
        | Block block -> visit_block block
        | Lit _ -> ()
        | StructExpr { struct_name; fields; _ } ->
            visit_path struct_name mdl (Some Type);
            fields#iter (fun (_, expr) -> visit_expr expr mdl)
        | Field (expr, _) -> visit_expr expr mdl
        | Cast (expr, ty) ->
            visit_expr expr mdl;
            resolve_ty ty
        | MethodCall (expr, seg, args) ->
            visit_expr expr mdl;
            visit_segment seg;
            args#iter (fun expr -> visit_expr expr mdl)
        | Match (expr, arms) ->
            visit_expr expr mdl;
            arms#iter (fun { pat; expr; _ } ->
                match expr.expr_kind with
                | Block block ->
                    let mdl' =
                      modules#unsafe_get (local_def_id block.block_id)
                    in
                    visit_pat pat mdl';
                    visit_expr expr mdl
                | _ -> assert false)
        | Slice exprs -> exprs#iter (fun expr -> visit_expr expr mdl)
        | Index (expr, idx) ->
            visit_expr expr mdl;
            visit_expr idx mdl
      and visit_pat ?(env = new hashmap) pat mdl =
        match pat with
        | PIdent (m, name, id) when not @@ env#has name ->
            let segments = new vec in
            let id = !id in
            segments#push
              { ident = name; args = None; id; span = Source.Span.dummy };
            let path =
              { segments; path_id = id; span = Source.Span.dummy }
            in
            let res = self#resolve_path mdl path (Some Type) in
            ignore @@ tcx#res_map#insert id res;
            (match res with
             | Def (_, Cons) -> ()
             | Def _ | Err ->
                 assert (env#insert name id = None);
                 let res = Res (Local (tcx#ast_mut_to_mut m, id)) in
                 self#shadow mdl name Value res
             | _ -> assert false)
        | PIdent (_, name, id) -> id := env#unsafe_get name
        | PCons (path, subpats) ->
            visit_path path mdl (Some Value);
            subpats#iter (fun pat -> visit_pat ~env pat mdl)
        | PPath path -> visit_path path mdl (Some Value)
        | POr subpats -> subpats#iter (fun pat -> visit_pat ~env pat mdl)
        | PWild | PLit _ | PRange _ -> ()
      and visit_block body =
        let mdl = modules#unsafe_get (local_def_id body.block_id) in
        body.block_stmts#iter (fun stmt ->
            match stmt with
            | Stmt expr | Expr expr -> visit_expr expr mdl
            | Binding { binding_pat; binding_expr; binding_ty; _ } ->
                (match binding_ty with
                 | Some ty -> resolve_ty ty
                 | None -> ());
                visit_expr binding_expr mdl;
                visit_pat binding_pat mdl
            | Assert (expr, _) -> visit_expr expr mdl
            | Assign (expr1, expr2) ->
                visit_expr expr1 mdl;
                visit_expr expr2 mdl);
        match body.last_expr with
        | Some expr -> visit_expr expr mdl
        | None -> ()
      in
      let visit_fn (func : func) =
        with_generics_params self func.fn_generics (fun () ->
            func.fn_sig.args#iter (fun { ty; _ } -> resolve_ty ty);
            (match func.fn_sig.ret_ty with
             | Some ty -> resolve_ty ty
             | None -> ());
            match func.body with Some body -> visit_block body | None -> ())
      in
      let visit_assoc_fn ty fn =
        let did = local_def_id fn.func_id in
        tcx#define_assoc_fn ty fn.name did;
        visit_fn fn
      in
      let visit_struct (strukt : strukt) =
        with_generics_params self strukt.generics (fun () ->
            strukt.fields#iter (fun (ty, _) -> resolve_ty ty))
      in
      let visit_variant (variant : variant) =
        variant.fields#iter resolve_ty
      in
      let visit_adt (adt : adt) =
        with_generics_params self adt.generics (fun () ->
            adt.variants#iter visit_variant)
      in
      let visit_impl { ty; generics; items; _ } =
        with_generics_params self generics (fun () ->
            resolve_ty ty;
            match tcx#ast_ty_to_res ty with
            | Some res ->
                current_impl <- Some res;
                items#iter (function AssocFn fn -> visit_assoc_fn res fn);
                current_impl <- None
            | None -> assert false)
      in
      let visit_item (item : item) =
        match item with
        | Mod { resolved_mod; _ } ->
            (match resolved_mod with
             | Some modd ->
                 let id = local_def_id modd.mod_id in
                 let mdl = modules#unsafe_get id in
                 self#resolve_paths mdl modd
             | None -> ())
        | Fn (func, _) -> visit_fn func
        | Type (Struct strukt) -> visit_struct strukt
        | Type (Adt adt) -> visit_adt adt
        | Foreign (fns, _) -> fns#iter (fun f -> visit_fn f)
        | Impl impl -> visit_impl impl
        | Using using ->
            let path = using.prefix in
            let res =
              match using.kind with
              | Val (name, span) ->
                  visit_path path mdl (Some Type);
                  let res = tcx#res_map#unsafe_get path.path_id in
                  let did = binding_to_def_id (Res res) in
                  let mdl = modules#unsafe_get did in
                  let key =
                    { ident = name; disambiguator = 0; ns = Value }
                  in
                  if not @@ mdl.resolutions#has key
                  then self#not_found_in_module path name span;
                  res
              | Glob ->
                  visit_path path mdl (Some Type);
                  tcx#res_map#unsafe_get path.path_id
              | _ ->
                  let segments = path.segments#copy in
                  let path = { path with segments } in
                  let source = path.segments#pop.ident in
                  visit_path path mdl (Some Type);
                  let res = tcx#res_map#unsafe_get path.path_id in
                  let did = binding_to_def_id (Res res) in
                  let mdl = modules#unsafe_get did in
                  let key0 =
                    { ident = source; disambiguator = 0; ns = Value }
                  in
                  let key1 = { key0 with ns = Type } in
                  if not
                     @@ (mdl.resolutions#has key0 || mdl.resolutions#has key1)
                  then self#not_found using.prefix;
                  res
            in
            let import = imports#unsafe_get path.path_id in
            import.res <- res
        | ExternMod _ -> ()
      in
      modd.items#iter visit_item
  end
