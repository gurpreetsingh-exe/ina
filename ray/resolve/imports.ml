open Front
open Ast
open Printf
open Sema

type sess = { env : (path, lang_item) Hashtbl.t }

let sess = { env = Hashtbl.create 0 }

type resolver = {
  modd : modd;
  func_map : (ident, func) Hashtbl.t;
}

let resolver_create modd = { modd; func_map = Hashtbl.create 0 }

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

let rec import (resolver : resolver) (path : path) =
  let s = List.hd path.segments in
  let lib_path =
    if mod_exists resolver s then create_path resolver s
    else if Array.mem s (Sys.readdir "library") then
      find_std (String.concat Filename.dir_sep ["library"; s])
    else raise Not_found
  in
  let f lib_path =
    let ic = open_in lib_path in
    let src = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let tokenizer = Tokenizer.tokenize lib_path src in
    let pctx = Parser.parse_ctx_create tokenizer src in
    let modd = Parser.parse_mod pctx in
    let env, modd = (resolve { modd; func_map = Hashtbl.create 0 }, modd) in
    let infer_ctx = Infer.infer_ctx_create env in
    ignore (Infer.infer_begin infer_ctx modd);
    (env, modd)
  in
  let env, modd = f lib_path in
  let modd =
    List.fold_left
      (fun h next ->
        if not (Hashtbl.mem h.imported_mods next) then (
          printf "error: %s not found\n" next;
          assert false)
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
  let f item =
    match item with
    | Import path ->
        let env2, modd = import resolver path in
        Hashtbl.add resolver.modd.imported_mods modd.mod_name modd;
        (* sus *)
        if not (Hashtbl.mem env path) then Hashtbl.add env path (Mod modd);
        Hashtbl.iter
          (fun path item ->
            if not (Hashtbl.mem env path) then Hashtbl.add env path item)
          env2
    | Fn (fn, _) ->
        Hashtbl.add resolver.func_map fn.fn_sig.name fn;
        let path = { segments = abs_path @ [fn.fn_sig.name] } in
        resolve_body fn.body resolver;
        fn.func_path <- Some path;
        Hashtbl.add env path (Fn fn);
        if fn.is_extern then
          Hashtbl.add env { segments = [fn.fn_sig.name] } (Fn fn)
    | _ -> ()
  in
  List.iter f resolver.modd.items;
  env

and resolve_body body resolver =
  let rec handle_expr expr =
    match expr.expr_kind with
    | Call (path, exprs) ->
        let st = List.hd path.segments in
        if Hashtbl.mem resolver.func_map st then (
          let abs_path = get_abs_ray_path resolver.modd.mod_path in
          let segs = abs_path @ path.segments in
          path.segments <- segs)
        else if Hashtbl.mem resolver.modd.imported_mods st then (
          let modd = Hashtbl.find resolver.modd.imported_mods st in
          let abs_path = get_abs_ray_path modd.mod_path in
          path.segments <- List.tl path.segments;
          let segs = abs_path @ path.segments in
          path.segments <- segs);
        List.iter handle_expr exprs
    | Binary (_, left, right) -> handle_expr left; handle_expr right
    | Deref expr | Ref expr -> handle_expr expr
    | Block body -> resolve_body (Some body) resolver
    | Path _ -> ()
    | If { cond; then_block; else_block } -> (
        handle_expr cond;
        resolve_body (Some then_block) resolver;
        match else_block with Some expr -> handle_expr expr | None -> ())
    | Lit _ -> ()
  in
  let f stmt =
    match stmt with
    | Binding { binding_expr; _ } -> handle_expr binding_expr
    | Assign (expr1, expr2) -> handle_expr expr1; handle_expr expr2
    | Stmt expr | Expr expr -> handle_expr expr
  in
  match body with
  | Some body -> (
      List.iter f body.block_stmts;
      match body.last_expr with Some expr -> handle_expr expr | None -> ())
  | None -> ()

let print_env env =
  let ritem = function Mod _ -> "module" | Fn _ -> "fn" in
  Hashtbl.iter
    (fun path item ->
      printf "%s = %s\n" (String.concat "::" path.segments) (ritem item))
    env
