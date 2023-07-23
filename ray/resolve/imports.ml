open Front
open Ast
open Ty
open Printf
open Sema
open Session

type sess = { env : (path, lang_item) Hashtbl.t }

let print_env env =
  let ritem = function
    | Mod _ -> "module"
    | Fn _ -> "fn"
    | Struct _ -> "struct"
  in
  Hashtbl.iter
    (fun path item ->
      printf "%s = %s\n" (String.concat "::" path.segments) (ritem item))
    env

let sess = { env = Hashtbl.create 0 }

type env = {
  mutable parent : env option;
  mutable bindings : string array;
}

let rec find_ty (env : env) (ident : string) : bool =
  if Array.mem ident env.bindings then true
  else (
    match env.parent with Some env -> find_ty env ident | None -> false)

type resolver = {
  globl_ctx : Context.t;
  modd : modd;
  func_map : (ident, func) Hashtbl.t;
  struct_map : (ident, strukt) Hashtbl.t;
  mutable env : env;
  mutable imported_names : string array;
}

let resolver_create globl_ctx modd =
  {
    globl_ctx;
    modd;
    func_map = Hashtbl.create 0;
    struct_map = Hashtbl.create 0;
    env = { parent = None; bindings = [||] };
    imported_names = [||];
  }

let rec mod_exists resolver name =
  let path =
    String.concat Filename.dir_sep
      [Filename.dirname resolver.modd.mod_path; name]
  in
  if Sys.file_exists path then true
  else if Filename.extension name <> ".ray" then
    mod_exists resolver (name ^ ".ray")
  else false

let create_path resolver name =
  if Sys.file_exists name then
    if Sys.is_directory name then
      String.concat Filename.dir_sep [name; "lib.ray"]
    else name ^ ".ray"
  else if Sys.file_exists (name ^ ".ray") then name ^ ".ray"
  else (
    let path =
      String.concat Filename.dir_sep
        [Filename.dirname resolver.modd.mod_path; name]
    in
    let f path =
      if Sys.is_directory path then
        String.concat Filename.dir_sep [path; "lib.ray"]
      else path
    in
    let path =
      if Sys.file_exists path then f path
      else if Sys.file_exists (path ^ ".ray") then f (path ^ ".ray")
      else raise Not_found
    in
    path)

let find_std lib =
  if Sys.is_directory lib then (
    let lib_path = String.concat Filename.dir_sep [lib; "lib.ray"] in
    if Sys.file_exists lib_path then lib_path else raise Not_found)
  else raise Not_found

let rec get_abs_path path =
  let parent = Filename.dirname path in
  if Array.mem "lib.ray" (Sys.readdir parent) then (
    let base = Filename.basename path in
    if base = "lib.ray" then get_abs_path parent
    else get_abs_path parent @ [Filename.remove_extension base])
  else [Filename.remove_extension (Filename.basename path)]

let rec get_abs_ray_path mod_path =
  let parent = Filename.dirname mod_path in
  let mod_name = Filename.basename (Filename.remove_extension mod_path) in
  match mod_name with
  | "lib" -> get_abs_ray_path parent
  | _ ->
      if Array.mem "lib.ray" (Sys.readdir parent) then
        get_abs_ray_path parent @ [mod_name]
      else [mod_name]

let handle_path resolver path =
  let st = List.hd path.segments in
  if Hashtbl.mem resolver.func_map st then (
    let abs_path = get_abs_ray_path resolver.modd.mod_path in
    let segs = abs_path @ path.segments in
    path.segments <- segs)
  else if Hashtbl.mem resolver.struct_map st then (
    let abs_path = get_abs_ray_path resolver.modd.mod_path in
    let segs = abs_path @ path.segments in
    path.segments <- segs)
  else if Hashtbl.mem resolver.modd.imported_mods st then (
    let modd = Hashtbl.find resolver.modd.imported_mods st in
    let abs_path = get_abs_ray_path modd.mod_path in
    path.segments <- List.tl path.segments;
    let segs = abs_path @ path.segments in
    path.segments <- segs)

let rec resolve_type resolver (env : (path, lang_item) Hashtbl.t) ty name =
  match ty with
  | Ident path ->
      handle_path resolver path;
      let strukt =
        if Hashtbl.mem env path then (
          let item = Hashtbl.find env path in
          match item with Struct strukt -> strukt | _ -> assert false)
        else (
          eprintf "error: `%s` not found\n" (Fmt.render_path path);
          flush stderr;
          exit 1)
      in
      let ty =
        Struct
          ( Fmt.render_path (Option.get strukt.struct_path),
            List.map (fun (ty, field) -> (field, ty)) strukt.members )
      in
      ty
  | Ptr ty -> Ptr (resolve_type resolver env ty name)
  | RefTy ty -> RefTy (resolve_type resolver env ty name)
  | FnTy (tys, ret_ty, is_variadic) ->
      FnTy
        ( List.map (fun ty -> resolve_type resolver env ty name) tys,
          resolve_type resolver env ret_ty name,
          is_variadic )
  | _ -> ty

let resolve_types resolver (env : (path, lang_item) Hashtbl.t) fields =
  List.map
    (fun (ty, name) -> (resolve_type resolver env ty name, name))
    fields

let rec import (resolver : resolver) (path : path) =
  let s = List.hd path.segments in
  let lib_path =
    if mod_exists resolver s then create_path resolver s
    else if Array.mem s (Sys.readdir "library") then
      find_std (String.concat Filename.dir_sep ["library"; s])
    else (
      eprintf "error: module `%s` not found\n" s;
      flush stderr;
      exit 1)
  in
  let f lib_path =
    let ic = open_in lib_path in
    let src = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let tokenizer = Tokenizer.tokenize lib_path src in
    let pctx = Parser.parse_ctx_create resolver.globl_ctx tokenizer src in
    let modd = Parser.parse_mod pctx in
    resolver.imported_names <-
      Array.append resolver.imported_names [|modd.mod_name|];
    let env, modd =
      ( resolve
          {
            globl_ctx = resolver.globl_ctx;
            modd;
            func_map = Hashtbl.create 0;
            struct_map = Hashtbl.create 0;
            env = { parent = None; bindings = [||] };
            imported_names = resolver.imported_names;
          },
        modd )
    in
    let infer_ctx =
      Infer.infer_ctx_create pctx.emitter resolver.globl_ctx env
    in
    ignore (Infer.infer_begin infer_ctx modd);
    let ty_ctx = Tychk.ty_ctx_create infer_ctx in
    ignore (Tychk.tychk ty_ctx modd);
    (env, modd)
  in
  let env, modd = f lib_path in
  let modd =
    List.fold_left
      (fun h next ->
        if not (Hashtbl.mem h.imported_mods next) then (
          eprintf "error: `%s` not found in `%s`\n" next h.mod_name;
          flush stderr;
          exit 1)
        else (
          let path =
            String.concat Filename.dir_sep
              [Filename.dirname h.mod_path; next]
          in
          let lib_path = create_path resolver path in
          let _, h = f lib_path in
          h))
      modd (List.tl path.segments)
  in
  (env, modd)

and resolve resolver : (path, lang_item) Hashtbl.t =
  let env = Hashtbl.create 0 in
  let abs_path = get_abs_path resolver.modd.mod_path in
  Hashtbl.add env { segments = abs_path } (Mod resolver.modd);
  let resolve_func fn =
    List.iter
      (fun (_, ident) ->
        resolver.env.bindings <- Array.append resolver.env.bindings [|ident|])
      fn.fn_sig.args;
    Hashtbl.add resolver.func_map fn.fn_sig.name fn;
    let path = { segments = abs_path @ [fn.fn_sig.name] } in
    resolve_body fn.body resolver env;
    fn.func_path <- Some path;
    Hashtbl.add env path (Fn fn);
    fn.fn_sig.args <- resolve_types resolver env fn.fn_sig.args
  in
  let f item =
    match item with
    | Import path ->
        let name = List.nth path.segments (List.length path.segments - 1) in
        if not (Array.mem name resolver.imported_names) then (
          let env2, modd = import resolver path in
          Hashtbl.add resolver.modd.imported_mods modd.mod_name modd;
          Hashtbl.add env { segments = [name] } (Mod modd);
          (* sus *)
          if not (Hashtbl.mem env path) then Hashtbl.add env path (Mod modd);
          Hashtbl.iter
            (fun path item ->
              if not (Hashtbl.mem env path) then Hashtbl.add env path item)
            env2)
    | Fn (fn, _) -> resolve_func fn
    | Foreign funcs -> List.iter resolve_func funcs
    | Type (Struct s) ->
        Hashtbl.add resolver.struct_map s.ident s;
        let path = { segments = abs_path @ [s.ident] } in
        s.struct_path <- Some path;
        Hashtbl.add env path (Struct s);
        s.members <- resolve_types resolver env s.members
    | Const _ -> ()
  in
  List.iter f resolver.modd.items;
  env

and resolve_body body resolver env =
  let rec handle_expr expr =
    match expr.expr_kind with
    | Call (path, exprs) ->
        handle_path resolver path;
        List.iter handle_expr exprs
    | Binary (_, left, right) -> handle_expr left; handle_expr right
    | Deref expr | Ref expr -> handle_expr expr
    | Block body -> resolve_body (Some body) resolver env
    | Path path ->
        let name = Fmt.render_path path in
        if not (find_ty resolver.env name) then handle_path resolver path
    | If { cond; then_block; else_block } -> (
        handle_expr cond;
        resolve_body (Some then_block) resolver env;
        match else_block with Some expr -> handle_expr expr | None -> ())
    | Lit _ -> ()
    | StructExpr { fields; struct_name } ->
        handle_path resolver struct_name;
        List.iter (fun (_, expr) -> handle_expr expr) fields
    | Cast (expr_, ty) ->
        handle_expr expr_;
        let ty = resolve_type resolver env ty "" in
        expr.expr_kind <- Cast (expr_, ty)
    | Field (expr, _) -> handle_expr expr
  in
  let f stmt =
    match stmt with
    | Binding { binding_pat; binding_expr; _ } ->
        (match binding_pat with
        | PatIdent ident ->
            resolver.env.bindings <-
              Array.append resolver.env.bindings [|ident|]);
        handle_expr binding_expr
    | Assign (expr1, expr2) -> handle_expr expr1; handle_expr expr2
    | Stmt expr | Expr expr | Assert (expr, _) -> handle_expr expr
  in
  match body with
  | Some body ->
      let tmp = resolver.env in
      resolver.env <- { parent = Some tmp; bindings = [||] };
      List.iter f body.block_stmts;
      (match body.last_expr with
      | Some expr -> handle_expr expr
      | None -> ());
      resolver.env <- tmp
  | None -> ()
