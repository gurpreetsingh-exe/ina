open Printf

let rec dfs graph node used idx =
  Hashtbl.replace used node 0;
  if not (Hashtbl.mem graph node) then assert false;
  List.iter
    (fun id ->
      if Hashtbl.mem used id && (not @@ List.mem id !idx)
      then (
        print_endline "cycle detected";
        assert false);
      if not (Hashtbl.mem used id) then dfs graph id used idx)
    (Hashtbl.find graph node);
  if not @@ List.mem node !idx then idx := !idx @ [node]
;;

let modulize m = String.capitalize_ascii @@ Filename.remove_extension m

module Path = struct
  type t = string

  let join segments : t = String.concat Filename.dir_sep segments
  let with_ext path ext = Filename.remove_extension path ^ ext
end

module Arg = struct
  type t =
    | Include of string
    | Impl of string
    | Lib of string

  let write = function
    | Include inc -> "-I " ^ inc
    | Impl imp -> Path.with_ext imp ".ml"
    | Lib lib -> lib
  ;;
end

module Library = struct
  type t = {
      path: Path.t
    ; modules: string list
    ; includes: string list
    ; is_single_unit: bool
    ; mutable depencency_order: string list
  }

  let dirname lib =
    if lib.is_single_unit then lib.path else Filename.dirname lib.path
  ;;

  let get_output_path lib =
    if not lib.is_single_unit
    then
      Path.with_ext (Path.join [lib.path; Filename.basename lib.path]) ".a"
    else Path.with_ext (Path.join [lib.path; List.hd lib.modules]) ".a"
  ;;

  let get_include_dir lib m =
    let dir = Path.join [dirname lib; m] in
    if Sys.file_exists dir then dir else Filename.dirname dir
  ;;

  let resolve_dependencies lib graph =
    let modules = List.map modulize lib.modules in
    List.iter
      (fun m ->
        let unit = Path.join [lib.path; m] in
        let m = modulize m in
        let command = sprintf "ocamldep -modules %s" unit in
        let pipe = Unix.open_process_in command in
        let output = In_channel.input_all pipe in
        In_channel.close pipe;
        let split = String.split_on_char ':' output in
        let dep_names =
          Str.split (Str.regexp "[ \n\r\t]+") (List.hd (List.tl split))
        in
        let deps =
          List.filter (fun s -> s <> "" && List.mem s modules) dep_names
        in
        match Hashtbl.find_opt graph m with
        | Some deps_old -> Hashtbl.replace graph m (deps_old @ deps)
        | None -> Hashtbl.add graph m deps)
      lib.modules
  ;;

  let resolve lib =
    let graph = Hashtbl.create 0 in
    resolve_dependencies lib graph;
    let order = ref [] in
    Hashtbl.iter (fun k v -> dfs graph k (Hashtbl.create 0) order) graph;
    lib.depencency_order <- !order;
    assert (List.length lib.depencency_order = List.length lib.modules)
  ;;

  let load path includes =
    if Sys.is_directory path
    then
      let children = Array.to_list @@ Sys.readdir path in
      let modules =
        List.filter (fun c -> Filename.extension c = ".ml") children
      in
      {
        path
      ; modules
      ; includes
      ; is_single_unit = false
      ; depencency_order = []
      }
    else
      {
        path = Filename.dirname path
      ; modules = [Filename.basename path]
      ; includes
      ; is_single_unit = true
      ; depencency_order = [modulize @@ Filename.basename path]
      }
  ;;
end

let compile (lib : Library.t) options =
  let modules =
    List.map
      (fun m ->
        Path.join
          [lib.path; Path.with_ext (String.uncapitalize_ascii m) ".ml"])
      lib.depencency_order
  in
  (* print_endline @@ String.concat "->" modules; *)
  let includes =
    List.map (fun m -> "-I " ^ Library.get_include_dir lib m) lib.includes
  in
  let includes = includes @ List.map (fun o -> "-I " ^ o) options in
  let includes = String.concat " " includes in
  let output = Library.get_output_path lib in
  let pack = modulize @@ Filename.basename output in
  List.iter
    (fun unit ->
      let command =
        sprintf
          "ocamlopt -I %s %s -for-pack %s -c %s"
          lib.path
          includes
          pack
          unit
      in
      print_endline command;
      if Sys.command command <> 0 then assert false)
    modules;
  if not lib.is_single_unit
  then (
    let mods =
      List.map
        (fun m ->
          Path.join
            [lib.path; Path.with_ext (String.uncapitalize_ascii m) ".cmx"])
        lib.depencency_order
    in
    let pack =
      Path.join [Filename.dirname output; Path.with_ext pack ".cmx"]
    in
    let mods = String.concat " " mods in
    let command = sprintf "ocamlopt %s -pack -o %s %s" includes pack mods in
    print_endline command;
    if Sys.command command <> 0 then print_endline "failed")
;;

let build_dir = "_build2"

let libs =
  [
    "source", []
  ; "metadata", []
  ; "token.ml", ["source"]
  ; "errors", ["token"; "source"]
  ; "utils", ["errors"]
  ; "session", ["metadata"; "source"; "errors"]
  ; "ty.ml", ["token"; "session"; "metadata"]
  ; "ast.ml", ["ty"; "source"]
  ; "front", ["token"; "ast"; "source"; "errors"; "session"]
  ; ( "resolve"
    , ["ast"; "utils"; "front"; "metadata"; "ty"; "errors"; "session"] )
  ; "ir", ["ty"]
  ; "lowering", ["ty"; "ast"; "ir"; "front"; "source"; "session"]
  ; "sema", ["ast"; "ty"; "errors"; "front"; "source"; "session"]
  ; "codegen", ["ty"; "ir"; "session"; "utils"; "metadata"]
  ; ( "driver"
    , [
        "front"
      ; "lowering"
      ; "resolve"
      ; "sema"
      ; "codegen"
      ; "session"
      ; "ir"
      ; "errors"
      ; "utils"
      ; "ty"
      ] )
  ]
;;

let extra_libs = ["llvm"]

let run () =
  (* TODO: custom build dir *)
  ignore @@ Sys.command ("rm -rf " ^ build_dir);
  if Sys.command ("mkdir -p " ^ build_dir) <> 0 then assert false;
  if Sys.command (sprintf "cp -rf ray %s/ray" build_dir) <> 0
  then assert false;
  let options =
    List.map
      (fun lib ->
        let command = sprintf "ocamlfind query %s" lib in
        let pipe = Unix.open_process_in command in
        let output = Option.get @@ In_channel.input_line pipe in
        In_channel.close pipe;
        output)
      extra_libs
  in
  List.iter
    (fun (lib, deps) ->
      let lib = Library.load (Path.join [build_dir; "ray"; lib]) deps in
      if not lib.is_single_unit then Library.resolve lib;
      compile lib options)
    libs
;;

let () = run ()
