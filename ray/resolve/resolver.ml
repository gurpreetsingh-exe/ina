open Printf
open Ast
open Ty
open Utils
open Front

type namespace =
  | Type
  | Value

and binding_key = {
  ident : string;
  ns : namespace;
}

and modul = {
  parent : modul option;
  resolutions : resolutions;
}

and name_binding_kind =
  | Res of res
  | Module of modul

and name_resolution = { binding : name_binding option }

and name_binding = { kind : name_binding_kind }

and resolutions = (binding_key, name_resolution) Hashtbl.t

let add_name_res resolutions key res = Hashtbl.replace resolutions key res

let print_binding_key key =
  let ns = function Type -> "type" | Value -> "value" in
  sprintf "%s" key.ident

let print_res = function
  | Def (id, kind) ->
      sprintf "(%s~%s)" (print_def_kind kind) (print_def_id id)
  | PrimTy ty -> print_prim_ty ty
  | Err -> "err"

let rec print_nameres (res : name_resolution) (depth : int) =
  match res.binding with
  | Some { kind } -> (
    match kind with
    | Res res -> print_res res
    | Module modul -> "\n" ^ print_resolutions modul.resolutions (depth + 1))
  | None -> "NONE"

and print_resolutions (res : resolutions) (depth : int) =
  let indent = String.make (depth * 4) ' ' in
  let print_pair (key, res) =
    sprintf "%s%s: %s" indent (print_binding_key key)
      (print_nameres res depth)
  in
  Hashtbl.to_seq res |> Array.of_seq |> Array.map print_pair |> Array.to_list
  |> String.concat "\n"

type t = {
  tcx : tcx;
  mutable modd : modd;
  root : modd;
  mutable is_root : bool;
}

let create tcx modd = { tcx; modd; root = modd; is_root = true }

let rec mod_exists resolver name =
  let f path =
    if Sys.file_exists (path ^ ".ray") then true
    else if Sys.file_exists path then true
    else if Filename.extension name <> ".ray" then
      mod_exists resolver (name ^ ".ray")
    else false
  in
  f
    (if resolver.is_root then
     Path.join [Filename.dirname resolver.modd.mod_path; name]
    else
      Path.join
        [
          Filename.dirname resolver.modd.mod_path;
          resolver.modd.mod_name;
          name;
        ])

let create_path resolver name =
  let f name =
    if Sys.file_exists (name ^ ".ray") then name ^ ".ray"
    else if Sys.file_exists name then
      if Sys.is_directory name then Path.join [name; "lib.ray"]
      else name ^ ".ray"
    else (
      let path = Path.join [Filename.dirname resolver.modd.mod_path; name] in
      let f path =
        if Sys.is_directory path then Path.join [path; "lib.ray"] else path
      in
      let path =
        if Sys.file_exists (path ^ ".ray") then f (path ^ ".ray")
        else if Sys.file_exists path then f path
        else raise Not_found
      in
      path)
  in
  f
    (if resolver.is_root then name
    else
      Path.join
        [
          Filename.dirname resolver.modd.mod_path;
          resolver.modd.mod_name;
          name;
        ])

let fn_ty func =
  let { fn_sig = { args; ret_ty; is_variadic; _ }; _ } = func in
  let ret_ty = Option.value ret_ty ~default:Unit in
  FnTy (List.map (fun (ty, _) -> ty) args, ret_ty, is_variadic)

let struct_ty (strukt : strukt) =
  Struct
    (strukt.ident, List.map (fun (ty, field) -> (field, ty)) strukt.members)

let rec resolve resolver : resolutions =
  let resolutions = Hashtbl.create 0 in
  let modul = { parent = None; resolutions } in
  let visit_item (item : item) =
    match item with
    | Mod m ->
        let name = m.name in
        if not (mod_exists resolver name) then (
          eprintf "error: module `%s` not found\n" name;
          flush stderr)
        else (
          let path = create_path resolver name in
          let ic = open_in path in
          let src = really_input_string ic (in_channel_length ic) in
          close_in ic;
          let tokenizer = Tokenizer.tokenize path src in
          let pctx = Parser.parse_ctx_create resolver.tcx tokenizer src in
          let modd = Parser.parse_mod pctx in
          let tmp_mod = resolver.modd in
          let tmp_root = resolver.is_root in
          resolver.modd <- modd;
          resolver.is_root <- false;
          let res = resolve resolver in
          resolver.modd <- tmp_mod;
          resolver.is_root <- tmp_root;
          let key = { ident = modd.mod_name; ns = Type } in
          let modul = { parent = Some modul; resolutions = res } in
          let binding = { binding = Some { kind = Module modul } } in
          add_name_res resolutions key binding;
          m.resolved_mod <- Some modd)
    | Fn (func, _) ->
        let id = { inner = func.func_id } in
        let res = Def (id, Fn) in
        create_def resolver.tcx id (Ty (fn_ty func));
        let key = { ident = func.fn_sig.name; ns = Value } in
        let binding = { binding = Some { kind = Res res } } in
        add_name_res resolutions key binding
    | Type (Struct s) ->
        let id = { inner = s.struct_id } in
        let res = Def (id, Struct) in
        create_def resolver.tcx id (Ty (struct_ty s));
        let key = { ident = s.ident; ns = Type } in
        let binding = { binding = Some { kind = Res res } } in
        add_name_res resolutions key binding
    | Foreign _ -> ()
    | Const _ | Import _ -> ()
  in
  List.iter visit_item resolver.modd.items;
  resolutions
(* resolver.namespaces <- Array.append resolver.namespaces [|ns|]; *)
(* printf "%s" (print_ns ns) *)
