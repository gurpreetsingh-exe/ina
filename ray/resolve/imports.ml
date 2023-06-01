open Front
open Ast
open Printf

type lang_item =
  | Mod of modd
  | Fn of func

type sess = { env : (path, lang_item) Hashtbl.t }

let sess = { env = Hashtbl.create 0 }

type resolver = { modd : modd }

let resolver_create modd = { modd }

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
  path

let std_path = "library/std/"

let find_std () =
  if Sys.is_directory std_path then (
    let lib_path = std_path ^ "lib.ray" in
    if Sys.file_exists lib_path then lib_path else raise Not_found)
  else raise Not_found

let rec get_abs_path path =
  let parent = Filename.dirname path in
  if Array.mem "lib.ray" (Sys.readdir parent) then (
    let base = Filename.basename path in
    if base = "lib.ray" then get_abs_path parent
    else get_abs_path parent @ [Filename.remove_extension base])
  else [Filename.remove_extension (Filename.basename path)]

let rec import (resolver : resolver) (path : path) =
  let s = List.hd path.segments in
  let lib_path =
    match s with
    | "std" -> find_std ()
    | _ ->
        if mod_exists resolver s then create_path resolver s
        else raise Not_found
  in
  let ic = open_in lib_path in
  let src = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let tokenizer = Tokenizer.tokenize lib_path src in
  let pctx = Parser.parse_ctx_create tokenizer src in
  let modd = Parser.parse_mod pctx in
  (* print_endline (Fmt.render_mod modd); *)
  resolve { modd }

and resolve resolver : (path, lang_item) Hashtbl.t =
  let env = Hashtbl.create 0 in
  let abs_path = get_abs_path resolver.modd.mod_path in
  printf "    %s = %s\n" (String.concat "::" abs_path) resolver.modd.mod_path;
  Hashtbl.add env { segments = abs_path } (Mod resolver.modd);
  let f item =
    match item with
    | Import path ->
        import resolver path
        |> Hashtbl.iter (fun path item ->
               Hashtbl.add env { segments = path.segments } item)
    | Fn (fn, _) ->
        Hashtbl.add env { segments = abs_path @ [fn.fn_sig.name] } (Fn fn)
    | _ -> ()
  in
  List.iter f resolver.modd.items;
  env

let print_env env =
  let ritem = function Mod _ -> "module" | Fn _ -> "fn" in
  Hashtbl.iter
    (fun path item ->
      printf "%s = %s\n" (String.concat "::" path.segments) (ritem item))
    env
