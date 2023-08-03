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

and disambiguator = { mutable stack : int list }

and binding_key = {
  ident : string;
  ns : namespace;
  disambiguator : int;
}

and name_binding = { kind : name_binding_kind (* vis : vis *) }

and name_resolution = { binding : name_binding }

and resolutions = (binding_key, name_resolution) Hashtbl.t

module Res = struct
  type t = res

  let modul binding : modul =
    match binding.kind with Res _ -> assert false | Module m -> m

  let res binding : res =
    match binding.kind with Res res -> res | Module _ -> assert false
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

let print_binding_key key =
  (* let ns = function Type -> "type" | Value -> "value" in *)
  sprintf "%s.%d" key.ident key.disambiguator

let rec print_nameres (res : name_resolution) (depth : int) =
  print_name_binding res.binding depth

and print_name_binding name_binding depth =
  match name_binding.kind with
  | Res res -> print_res res
  | Module modul -> "\n" ^ print_resolutions modul.resolutions (depth + 1)

and print_resolutions (res : resolutions) (depth : int) =
  let indent = String.make (depth * 2) ' ' in
  let print_pair (key, res) =
    sprintf "%s%s: %s" indent (print_binding_key key)
      (print_nameres res depth)
  in
  Hashtbl.to_seq res |> Array.of_seq |> Array.map print_pair |> Array.to_list
  |> String.concat "\n"

let print_resolutions res = print_endline (print_resolutions res 0)

let print_scope scope_table =
  let print_scope' _ =
    let print_pair (key, res) =
      sprintf "%s: %s" (print_binding_key key) (print_nameres res 0)
    in
    Hashtbl.to_seq scope_table
    |> Array.of_seq |> Array.map print_pair |> Array.to_list
    |> String.concat "\n"
  in
  print_endline (print_scope' ())

type t = {
  tcx : tcx;
  mutable modd : modd;
  mutable modul : modul option;
  scope_table : (binding_key, name_resolution) Hashtbl.t;
  mod_table : (def_id, modul) Hashtbl.t;
  mutable is_root : bool;
  disambiguator : disambiguator;
  mutable key : binding_key;
}

let create tcx modd =
  {
    tcx;
    modd;
    modul = None;
    scope_table = Hashtbl.create 0;
    mod_table = Hashtbl.create 0;
    is_root = true;
    disambiguator = { stack = [0] };
    key = { ident = ""; ns = Value; disambiguator = 0 };
  }

let add_mod resolver id modul = Hashtbl.replace resolver.mod_table id modul

module Disambiguator = struct
  let create disambiguator = List.hd disambiguator.stack

  let inc disambiguator =
    disambiguator.stack <-
      (match disambiguator.stack with
      | dg :: rest -> [dg + 1] @ rest
      | [] -> [0])

  let dec disambiguator =
    disambiguator.stack <-
      (match disambiguator.stack with
      | dg :: rest -> [dg - 1] @ rest
      | [] -> [0])

  let push disambiguator = disambiguator.stack <- [0] @ disambiguator.stack

  let pop disambiguator =
    disambiguator.stack <-
      (match disambiguator.stack with _ :: rest -> rest | [] -> [0])
end

let key resolver =
  let key = resolver.key in
  { key with disambiguator = Disambiguator.create resolver.disambiguator }

let add_scope_res resolver res binding =
  Hashtbl.replace resolver.scope_table res binding

let find_scope_res resolver key =
  let binding = Hashtbl.find resolver.scope_table key in
  Res.modul binding.binding

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

let rec resolve resolver : modul =
  let resolutions = Hashtbl.create 0 in
  let id = { inner = resolver.modd.mod_id } in
  let mkind = Def (Mod, id, resolver.modd.mod_name) in
  let modul = { mkind; parent = resolver.modul; resolutions } in
  add_mod resolver id modul;
  let rec visit_expr (expr : expr) (key : binding_key) (modul : modul) =
    match expr.expr_kind with
    | Binary (_, left, right) ->
        visit_expr left key modul;
        visit_expr right key modul
    | Block block ->
        let key =
          {
            key with
            disambiguator = Disambiguator.create resolver.disambiguator;
          }
        in
        Disambiguator.inc resolver.disambiguator;
        let m = visit_block block key (Some modul) in
        let binding = { binding = { kind = Module m } } in
        add_name_res modul.resolutions key binding
    | If { cond; then_block; else_block } -> (
        visit_expr cond key modul;
        let key =
          {
            key with
            disambiguator = Disambiguator.create resolver.disambiguator;
          }
        in
        Disambiguator.inc resolver.disambiguator;
        let m = visit_block then_block key (Some modul) in
        let binding = { binding = { kind = Module m } } in
        add_name_res modul.resolutions key binding;
        match else_block with
        | Some expr -> visit_expr expr key modul
        | None -> ())
    | Call (_, args) -> List.iter (fun e -> visit_expr e key modul) args
    | Field (expr, _) | Cast (expr, _) | Ref expr | Deref expr ->
        visit_expr expr key modul
    | StructExpr { fields; _ } ->
        List.iter (fun (_, e) -> visit_expr e key modul) fields
    | Lit _ | Path _ -> ()
  and visit_block (block : block) (key : binding_key) (parent : modul option)
      : modul =
    Disambiguator.push resolver.disambiguator;
    let modul = { mkind = Block; parent; resolutions = Hashtbl.create 0 } in
    let visit_stmt stmt =
      match stmt with
      | Binding { binding_expr; _ } -> visit_expr binding_expr key modul
      | Stmt e | Expr e | Assert (e, _) -> visit_expr e key modul
      | Assign (e1, e2) -> visit_expr e1 key modul; visit_expr e2 key modul
    in
    List.iter visit_stmt block.block_stmts;
    (match block.last_expr with
    | Some expr -> visit_expr expr key modul
    | None -> ());
    Disambiguator.pop resolver.disambiguator;
    modul
  in
  let visit_item (item : item) =
    match item with
    | Mod m -> (
        let name = m.name in
        let modd =
          if m.inline then m.resolved_mod
          else if not (mod_exists resolver name) then (
            eprintf "error(%s): module `%s` not found\n"
              resolver.modd.mod_path name;
            flush stderr;
            None)
          else (
            let path = create_path resolver name in
            let ic = open_in path in
            let src = really_input_string ic (in_channel_length ic) in
            close_in ic;
            let tokenizer = Tokenizer.tokenize path src in
            let pctx = Parser.parse_ctx_create resolver.tcx tokenizer src in
            Some (Parser.parse_mod pctx))
        in
        match modd with
        | Some modd ->
            let resolver =
              { resolver with modd; modul = Some modul; is_root = false }
            in
            let modul' = resolve resolver in
            let key =
              { ident = modd.mod_name; ns = Type; disambiguator = 0 }
            in
            let id = { inner = modd.mod_id } in
            let mkind = Def (Mod, id, modd.mod_name) in
            let modul =
              {
                mkind;
                parent = Some modul;
                resolutions = modul'.resolutions;
              }
            in
            let binding = { binding = { kind = Module modul } } in
            add_name_res resolutions key binding;
            m.resolved_mod <- Some modd
        | None -> ())
    | Fn (func, _) -> (
        let name = func.fn_sig.name in
        let segments = get_root_path modul @ [name] in
        let id = { inner = func.func_id } in
        let res : res = Def (id, Fn) in
        let path = { segments; res } in
        func.func_path <- Some path;
        create_def resolver.tcx id (Ty (fn_ty func)) path;
        let key = { ident = name; ns = Value; disambiguator = 0 } in
        let binding = { binding = { kind = Res res } } in
        add_name_res resolutions key binding;
        match func.body with
        | Some block ->
            let m = visit_block block key (Some modul) in
            let binding = { binding = { kind = Module m } } in
            add_scope_res resolver key binding
        | None -> ())
    | Type (Struct s) ->
        let name = s.ident in
        let id = { inner = s.struct_id } in
        let segments = get_root_path modul @ [name] in
        let res : res = Def (id, Struct) in
        let path = { segments; res } in
        create_def resolver.tcx id (Ty (struct_ty s)) path;
        let key = { ident = name; ns = Type; disambiguator = 0 } in
        let binding = { binding = { kind = Res res } } in
        add_name_res resolutions key binding
    | Foreign _ -> ()
    | Const _ | Import _ -> ()
  in
  List.iter visit_item resolver.modd.items;
  modul
