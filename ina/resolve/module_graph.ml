open Ast
open Resolver
open Middle.Def_id
open Structures.Hashmap
open Structures.Vec
open Errors.Diagnostic
open Module
open Front
open Printf
open Metadata.Decoder
open Utils
open Utils.Path

type mod_path = {
    path: string
  ; ownership: ownership
}

and ownership =
  | Owned of string option
  | Unowned

type mod_error =
  | ModNotFound of string * string * string
  | MultipleCandidates of string * string * string

let mod_error_emit tcx span = function
  | ModNotFound (name, def, sec) ->
      let msg = sprintf "file for `%s` not found" name in
      let sugg =
        sprintf
          "to create module `%s`, create one of `%s` or `%s`"
          name
          def
          sec
      in
      mk_err msg span |> tcx#emit
      (* let diag = *)
      (*   new diagnostic Err ~multi_span:(multi_span span) *)
      (*   |> message msg *)
      (*   |> help sugg *)
      (* in *)
      (* tcx#emit diag *)
  | MultipleCandidates (name, def, sec) ->
      let msg = sprintf "file for `%s` at `%s` and `%s`" name def sec in
      tcx#emit (Errors.Diagnostic.mk_err msg span)
;;

let print_mod_path { path; ownership } =
  sprintf
    "%s %s"
    path
    (match ownership with Owned (Some ident) -> ident | _ -> "None")
;;

let default_submodule_path ident relative dir =
  let sep = Filename.dir_sep in
  let relative_prefix =
    match relative with
    | Some relative -> sprintf "%s%s" relative sep
    | None -> ""
  in
  let default_path = join [dir; sprintf "%s%s.ina" relative_prefix ident] in
  let secondary_path =
    join [dir; sprintf "%s%s%smod.ina" relative_prefix ident sep]
  in
  let default_exists = Sys.file_exists default_path in
  let secondary_exists = Sys.file_exists secondary_path in
  match default_exists, secondary_exists with
  | true, false -> Ok { path = default_path; ownership = Owned (Some ident) }
  | false, true -> Ok { path = secondary_path; ownership = Owned None }
  | false, false -> Error (ModNotFound (ident, default_path, secondary_path))
  | true, true ->
      Error (MultipleCandidates (ident, default_path, secondary_path))
;;

let mod_file_path name ownership dir =
  let relative =
    match ownership with Owned relative -> relative | _ -> None
  in
  let result = default_submodule_path name relative dir in
  match ownership with Owned _ -> result | _ -> assert false
;;

class visitor resolver (modd : Ast.modd) parent dir_ownership =
  let create_modul () =
    let id = local_def_id modd.mod_id in
    let m =
      {
        mkind = Def (Mod, id, modd.mod_name)
      ; parent
      ; resolutions = new hashmap
      ; imports = new vec
      }
    in
    let _ = resolver#modules#insert id m in
    m
  in
  object (self)
    val resolver : resolver = resolver
    val modd = modd
    val parent = parent
    val mutable parent_id = local_def_id 0
    val mutable mdl = create_modul ()
    val mutable curr_fn = None
    val dir_ownership = dir_ownership
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
      | Cast (expr, _) | Field (expr, _) | Ref (_, expr) | Deref expr ->
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
        {
          mkind = Block
        ; parent = Some mdl
        ; resolutions = new hashmap
        ; imports = new vec
        }
      in
      (match curr_fn with
       | Some fn ->
           fn.fn_sig.args#iter (fun { arg; arg_id; _ } ->
               (* TODO: use patterns for arguments *)
               let res = Res (Local (Imm, arg_id)) in
               resolver#shadow mdl arg Value res);
           curr_fn <- None
       | None -> ());
      let id = local_def_id block.block_id in
      assert (resolver#modules#insert id mdl = None);
      self#with_module mdl (fun () ->
          block.block_stmts#iter self#visit_stmt;
          match block.last_expr with
          | Some expr -> self#visit_expr expr
          | None -> ())

    method visit_fn fn =
      let name = fn.name in
      resolver#tcx#insert_span fn.func_id fn.fn_sig.fn_span;
      let did = local_def_id fn.func_id in
      ignore (resolver#tcx#define parent_id did (ValueNs name));
      let res =
        Res (Def (did, if fn.abi = "intrinsic" then Intrinsic else Fn))
      in
      resolver#define mdl name Value res;
      self#visit_fn_sig fn.fn_sig;
      match fn.body with
      | Some body ->
          curr_fn <- Some fn;
          self#visit_block body
      | None -> ()

    method visit_assoc_fn fn =
      resolver#tcx#insert_span fn.func_id fn.fn_sig.fn_span;
      let did = local_def_id fn.func_id in
      ignore (resolver#tcx#define parent_id did (ValueNs fn.name));
      assert (not fn.is_extern);
      self#visit_fn_sig fn.fn_sig;
      match fn.body with
      | Some body ->
          curr_fn <- Some fn;
          self#visit_block body
      | None -> assert false

    method visit_impl (impl : impl) =
      let did = local_def_id impl.id in
      let did = resolver#tcx#define parent_id did (Impl did) in
      self#with_parent did (fun () ->
          impl.items#iter (function AssocFn fn -> self#visit_assoc_fn fn))

    method visit_struct ({ name; id; span; _ } : strukt) =
      resolver#tcx#insert_span id span;
      let did = local_def_id id in
      ignore (resolver#tcx#define parent_id did (TypeNs name));
      let res = Res (Def (did, Struct)) in
      resolver#define mdl name Type res

    method visit_adt (adt : adt) =
      resolver#tcx#insert_span adt.id adt.span;
      let did = local_def_id adt.id in
      ignore (resolver#tcx#define parent_id did (TypeNs adt.name));
      let m =
        {
          mkind = Def (Adt, did, adt.name)
        ; parent = Some mdl
        ; resolutions = new hashmap
        ; imports = new vec
        }
      in
      assert (resolver#modules#insert did m = None);
      let res = Module m in
      resolver#define mdl adt.name Type res;
      self#with_module m (fun () ->
          self#with_parent did (fun () ->
              adt.variants#iter (fun variant -> self#visit_variant variant)))

    method visit_variant { name; id; span; _ } =
      resolver#tcx#insert_span id span;
      let did = local_def_id id in
      ignore (resolver#tcx#define parent_id did (ValueNs name));
      let res = Res (Def (did, Cons)) in
      resolver#define mdl name Value res

    method visit_item item =
      match item with
      | Ast.Fn (fn, _) -> self#visit_fn fn
      | Type (Struct s) -> self#visit_struct s
      | Type (Adt adt) -> self#visit_adt adt
      | Foreign (fns, id) ->
          let did = local_def_id id in
          let did = resolver#tcx#define parent_id did ExternMod in
          self#with_parent did (fun () -> fns#iter self#visit_fn)
      | Impl impl -> self#visit_impl impl
      | Using using ->
          let segments = new vec in
          segments#copy using.prefix.segments;
          let path = { using.prefix with segments } in
          let ikind =
            match using.kind with
            | Val (name, _) -> Named { source = name; ns = Some Value }
            | Simple _ ->
                let source = path.segments#pop.ident in
                Named { source; ns = None }
            | Glob -> Glob
            | Nested _ -> assert false
          in
          let import = { ikind; path } in
          mdl.imports#push import
      | Mod m ->
          let f m o =
            let visitor = new visitor resolver m (Some mdl) o in
            visitor#with_parent parent_id (fun () ->
                visitor#visit_mod;
                let res = Module visitor#mdl in
                resolver#define mdl m.mod_name Type res)
          in
          (match m.resolved_mod with
           | Some m -> f m dir_ownership
           | None ->
               let dir = Filename.dirname modd.mod_path in
               (match mod_file_path m.name dir_ownership dir with
                | Ok mod_path ->
                    let res =
                      Parser.parse_mod_from_file
                        resolver#tcx#sess.parse_sess
                        mod_path.path
                    in
                    (match res with
                     | Ok parsed_module ->
                         m.resolved_mod <- Some parsed_module;
                         f parsed_module mod_path.ownership
                     | Error e -> resolver#tcx#emit e)
                | Error e -> mod_error_emit resolver#tcx m.span e))
      | ExternMod name ->
          (match resolver#tcx#extmods#has name with
           | true -> ()
           | _ ->
               let dir = Filename.dirname modd.mod_path in
               let lib = join [dir; sprintf "lib%s.o" name] in
               let lib =
                 if Sys.file_exists lib
                 then lib
                 else join ["library"; sprintf "lib%s.o" name]
               in
               resolver#tcx#append_extern_mod lib;
               let obj = Object.read_obj lib in
               let metadata =
                 Option.get @@ Object.read_section_by_name obj ".ina\000"
               in
               let extmod_id = resolver#tcx#unit name in
               let dec = new decoder metadata extmod_id in
               let name' = ref "" in
               decode_hashmap
                 dec
                 resolver#extmods
                 (fun dec ->
                   name' := dec#read_str;
                   !name')
                 (fun dec ->
                   let extmod_id = Hashtbl.hash !name' in
                   let extmod_id = def_id 0 extmod_id in
                   let dummy =
                     {
                       mkind =
                         Def (Mod, extmod_id, sprintf "__%s_wrapper" !name')
                     ; parent = None
                     ; resolutions = new hashmap
                     ; imports = new vec
                     }
                   in
                   let mdl' = Module.decode resolver dec None in
                   (match mdl'.mkind with
                    | Def (_, id, name) ->
                        ignore
                          (resolver#tcx#define extmod_id id (TypeNs name))
                    | _ -> assert false);
                   resolver#define dummy !name' Type (Module mdl');
                   dummy);
               resolver#tcx#decode_metadata dec;
               resolver#tcx#decoders#push dec)

    method with_parent def f =
      let tmp = parent_id in
      parent_id <- def;
      f ();
      parent_id <- tmp

    method visit_mod =
      let did = local_def_id modd.mod_id in
      let did = resolver#tcx#define parent_id did (TypeNs modd.mod_name) in
      self#with_parent did (fun () -> modd.items#iter self#visit_item)

    method visit_mod_root =
      let did = resolver#tcx#define_root (local_def_id 0) in
      self#with_parent did (fun () -> self#visit_mod)
  end
