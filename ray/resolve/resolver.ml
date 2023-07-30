open Printf
open Ast
open Ty
open Utils
open Front

type namespace =
  | Type
  | Value

and module_kind =
  | Block
  | Def of (def_kind * def_id * string)

and modul = {
  mkind : module_kind;
  parent : modul option;
  resolutions : resolutions;
}

and name_binding_kind =
  | Res of res
  | Module of modul

and binding_key = {
  ident : string;
  ns : namespace;
}

and name_binding = { kind : name_binding_kind (* vis : vis *) }

and name_resolution = { binding : name_binding option }

and resolutions = (binding_key, name_resolution) Hashtbl.t

module Res = struct
  type t = res

  let modul binding : modul =
    match binding.kind with Res _ -> assert false | Module m -> m
end

let rec get_root_path modul =
  let name = function Block -> assert false | Def (_, _, name) -> name in
  match modul.parent with
  | Some parent -> get_root_path parent @ [name modul.mkind]
  | None -> [name modul.mkind]

let add_name_res resolutions key res = Hashtbl.replace resolutions key res

let get_name_res (resolutions : resolutions) (key : binding_key) =
  if Hashtbl.mem resolutions key then Some (Hashtbl.find resolutions key)
  else None

let find_scope_res resolutions key =
  let name_binding_kind =
    (Option.get (Option.get (get_name_res resolutions key)).binding).kind
  in
  match name_binding_kind with
  | Res _ -> assert false
  | Module modd -> modd.resolutions

let print_binding_key key =
  (* let ns = function Type -> "type" | Value -> "value" in *)
  sprintf "%s" key.ident

let print_res : res -> string = function
  | Def (id, kind) ->
      sprintf "(%s~%s)" (print_def_kind kind) (print_def_id id)
  | PrimTy ty -> print_prim_ty ty
  | Local _ -> "local"
  | Err -> "err"

let rec print_nameres (res : name_resolution) (depth : int) =
  match res.binding with
  | Some { kind } -> (
    match kind with
    | Res res -> print_res res
    | Module modul -> "\n" ^ print_resolutions modul.resolutions (depth + 1))
  | None -> "NONE"

and print_resolutions (res : resolutions) (depth : int) =
  let indent = String.make (depth * 2) ' ' in
  let print_pair (key, res) =
    sprintf "%s%s: %s" indent (print_binding_key key)
      (print_nameres res depth)
  in
  Hashtbl.to_seq res |> Array.of_seq |> Array.map print_pair |> Array.to_list
  |> String.concat "\n"

type t = {
  tcx : tcx;
  mutable modd : modd;
  mutable modul : modul option;
  root : modd;
  scope_table : (res, name_binding) Hashtbl.t;
  mutable is_root : bool;
}

let create tcx modd =
  {
    tcx;
    modd;
    modul = None;
    root = modd;
    scope_table = Hashtbl.create 0;
    is_root = true;
  }

let add_scope_res resolver res binding =
  Hashtbl.replace resolver.scope_table res binding

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
  FnTy (List.map (fun (ty, _, _) -> ty) args, ret_ty, is_variadic)

let struct_ty (strukt : strukt) =
  Struct
    (strukt.ident, List.map (fun (ty, field) -> (field, ty)) strukt.members)

let rec resolve resolver : resolutions =
  let resolutions = Hashtbl.create 0 in
  let id = { inner = resolver.modd.mod_id } in
  let mkind = Def (Mod, id, resolver.modd.mod_name) in
  let modul = { mkind; parent = resolver.modul; resolutions } in
  let visit_block block resolutions =
    let visit_stmt stmt =
      match stmt with
      | Binding { binding_pat; binding_id; _ } -> (
          binding_pat
          |> function
          | PatIdent ident ->
              let key = { ident; ns = Value } in
              let binding =
                { binding = Some { kind = Res (Local binding_id) } }
              in
              add_name_res resolutions key binding)
      | Stmt _ | Expr _ -> ()
      | Assign _ -> ()
      | Assert _ -> ()
    in
    List.iter visit_stmt block.block_stmts;
    resolutions
  in
  let visit_item (item : item) =
    match item with
    | Mod m ->
        let name = m.name in
        if not (mod_exists resolver name) then (
          eprintf "error(%s): module `%s` not found\n" resolver.modd.mod_path
            name;
          flush stderr)
        else (
          let path = create_path resolver name in
          let ic = open_in path in
          let src = really_input_string ic (in_channel_length ic) in
          close_in ic;
          let tokenizer = Tokenizer.tokenize path src in
          let pctx = Parser.parse_ctx_create resolver.tcx tokenizer src in
          let modd = Parser.parse_mod pctx in
          let resolver =
            { resolver with modd; modul = Some modul; is_root = false }
          in
          let res = resolve resolver in
          let key = { ident = modd.mod_name; ns = Type } in
          let mkind = Def (Mod, { inner = modd.mod_id }, modd.mod_name) in
          let modul = { mkind; parent = Some modul; resolutions = res } in
          let binding = { binding = Some { kind = Module modul } } in
          add_name_res resolutions key binding;
          m.resolved_mod <- Some modd)
    | Fn (func, _) -> (
        let name = func.fn_sig.name in
        let segments = get_root_path modul @ [name] in
        let id = { inner = func.func_id } in
        let res : res = Def (id, Fn) in
        let path = { segments; res } in
        func.func_path <- Some path;
        create_def resolver.tcx id (Ty (fn_ty func)) path;
        let key = { ident = name; ns = Value } in
        let binding = { binding = Some { kind = Res res } } in
        add_name_res resolutions key binding;
        let resolutions = Hashtbl.create 0 in
        List.iter
          (fun (_, name, id) ->
            let key = { ident = name; ns = Value } in
            let binding = { binding = Some { kind = Res (Local id) } } in
            add_name_res resolutions key binding)
          func.fn_sig.args;
        match func.body with
        | Some block ->
            let resolutions = visit_block block resolutions in
            let modul =
              { mkind = Block; parent = Some modul; resolutions }
            in
            let binding = { kind = Module modul } in
            add_scope_res resolver res binding
        | None -> ())
    | Type (Struct s) ->
        let name = s.ident in
        let id = { inner = s.struct_id } in
        let segments = get_root_path modul @ [name] in
        let res : res = Def (id, Struct) in
        let path = { segments; res } in
        create_def resolver.tcx id (Ty (struct_ty s)) path;
        let key = { ident = name; ns = Type } in
        let binding = { binding = Some { kind = Res res } } in
        add_name_res resolutions key binding
    | Foreign _ -> ()
    | Const _ | Import _ -> ()
  in
  List.iter visit_item resolver.modd.items;
  resolutions
