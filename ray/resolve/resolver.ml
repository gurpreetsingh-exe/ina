open Printf
open Ast
open Ty
open Utils
open Front
open Metadata

let stdlib = "lib"

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
  scope_table : (binding_key, name_resolution) Hashtbl.t;
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

let name_binding_kind_to_enum = function Res _ -> 0L | Module _ -> 1L

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

let print_mkind = function
  | Block -> "block"
  | Def (_, id, ident) -> sprintf "%s%d" ident id.inner

type t = {
  tcx : tcx;
  mutable modd : modd;
  mutable modul : modul option;
  mutable unit_root : modul;
  mod_table : (def_id, modul) Hashtbl.t;
  mutable extern_units : modul list;
  mutable is_root : bool;
  disambiguator : disambiguator;
  mutable key : binding_key;
  units : (string, int) Hashtbl.t;
}

let create tcx modd =
  {
    tcx;
    modd;
    modul = None;
    unit_root =
      {
        mkind = Block;
        parent = None;
        resolutions = Hashtbl.create 0;
        scope_table = Hashtbl.create 0;
      };
    mod_table = Hashtbl.create 0;
    extern_units = [];
    is_root = true;
    disambiguator = { stack = [0] };
    key = { ident = ""; ns = Value; disambiguator = 0 };
    units = Hashtbl.create 0;
  }

let add_mod resolver id modul = Hashtbl.replace resolver.mod_table id modul

let add_unit resolver name =
  match Hashtbl.find_opt resolver.units name with
  | None ->
      let id = Hashtbl.length resolver.units + 1 in
      Hashtbl.add resolver.units name id;
      id
  | Some id -> id

let unit_id resolver name =
  match Hashtbl.find_opt resolver.units name with
  | Some id -> id
  | None -> add_unit resolver name

let rec encode_module enc modul =
  (match modul.mkind with
  | Def (_, def_id, name) ->
      Encoder.emit_u32 enc def_id.inner;
      Encoder.emit_str enc name
  | _ -> assert false);
  Encoder.emit_usize enc @@ Hashtbl.length modul.resolutions;
  Hashtbl.iter
    (fun key nameres ->
      Encoder.emit_str enc key.ident;
      Encoder.emit_u8 enc (match key.ns with Value -> 0 | Type -> 1);
      let kind = nameres.binding.kind in
      let dis = name_binding_kind_to_enum kind in
      match kind with
      | Res res -> Encoder.emit_with enc dis (fun e -> encode_res e res)
      | Module m -> Encoder.emit_with enc dis (fun e -> encode_module e m))
    modul.resolutions

let rec decode_module resolver dec parent =
  let def_id = def_id (Decoder.read_u32 dec) dec.unit_id in
  let name = Decoder.read_str dec in
  let modul =
    {
      mkind = Def (Mod, def_id, name);
      parent;
      resolutions = Hashtbl.create 0;
      scope_table = Hashtbl.create 0;
    }
  in
  let module_entries = Decoder.read_usize dec in
  List.iter
    (fun _ ->
      let ident = Decoder.read_str dec in
      let ns =
        match Decoder.read_u8 dec with
        | 0 -> Value
        | 1 -> Type
        | _ -> assert false
      in
      let key = { ident; ns; disambiguator = 0 } in
      let dis = Decoder.read_usize dec in
      let kind =
        match dis with
        | 0 -> Res (decode_res dec)
        | 1 -> Module (decode_module resolver dec (Some modul))
        | _ -> assert false
      in
      let binding = { binding = { kind } } in
      add_name_res modul.resolutions key binding)
    (List.init module_entries (fun x -> x));
  add_mod resolver def_id modul;
  modul

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

let add_scope_res scope_table res binding =
  Hashtbl.replace scope_table res binding

let find_scope_res scope_table key =
  let binding = Hashtbl.find scope_table key in
  Res.modul binding.binding

(* TODO: check for modules in subfolder *)
(* foo.ray:

   ``` mod bar; ```

   foo/bar.ray:

   ``` /// some stuff ```

   this doesn't work currently *)
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
    (if resolver.is_root then
     Path.join [Filename.dirname resolver.modd.mod_path; name]
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

let struct_ty (strukt : strukt) path =
  let name = render_path path in
  Struct (name, List.map (fun (ty, field) -> (field, ty)) strukt.members)

let rec resolve resolver root : modul =
  let id = def_id resolver.modd.mod_id 0 in
  let mkind = Def (Mod, id, resolver.modd.mod_name) in
  let modul =
    {
      mkind;
      parent = resolver.modul;
      resolutions = Hashtbl.create 0;
      scope_table = Hashtbl.create 0;
    }
  in
  if root then resolver.unit_root <- modul;
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
    let modul =
      {
        mkind = Block;
        parent;
        resolutions = Hashtbl.create 0;
        scope_table = Hashtbl.create 0;
      }
    in
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
  let visit_fn func modul =
    let name = func.fn_sig.name in
    let segments =
      if func.is_extern then [name] else get_root_path modul @ [name]
    in
    let mangled_name =
      if func.is_extern then name
      else
        "_Z"
        ^ String.concat ""
            (List.map
               (fun seg -> string_of_int (String.length seg) ^ seg)
               segments)
    in
    let id = def_id func.func_id 0 in
    let res : res =
      Def (id, if func.abi = "intrinsic" then Intrinsic else Fn)
    in
    let path = { segments; res } in
    func.func_path <- Some path;
    create_def resolver.tcx id (Ty (fn_ty func)) (Some mangled_name);
    let key = { ident = name; ns = Value; disambiguator = 0 } in
    let binding = { binding = { kind = Res res } } in
    add_name_res modul.resolutions key binding;
    match func.body with
    | Some block ->
        let m = visit_block block key (Some modul) in
        let binding = { binding = { kind = Module m } } in
        add_scope_res modul.scope_table key binding
    | None -> ()
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
            (* let resolver = *)
            (*   { resolver with modd; modul = Some modul; is_root = false } *)
            (* in *)
            let tmp_modd = resolver.modd in
            let tmp_modul = resolver.modul in
            let tmp_is_root = resolver.is_root in
            resolver.modd <- modd;
            resolver.modul <- Some modul;
            resolver.is_root <- false;
            let modul' = resolve resolver false in
            resolver.modd <- tmp_modd;
            resolver.modul <- tmp_modul;
            resolver.is_root <- tmp_is_root;
            let key =
              { ident = modd.mod_name; ns = Type; disambiguator = 0 }
            in
            let binding = { binding = { kind = Module modul' } } in
            add_name_res modul.resolutions key binding;
            m.resolved_mod <- Some modd
        | None -> ())
    | Fn (func, _) -> visit_fn func modul
    | Type (Struct s) ->
        let name = s.ident in
        let id = def_id s.struct_id 0 in
        let segments = get_root_path modul @ [name] in
        let res : res = Def (id, Struct) in
        let path = { segments; res } in
        create_def resolver.tcx id (Ty (struct_ty s path)) None;
        let key = { ident = name; ns = Type; disambiguator = 0 } in
        let binding = { binding = { kind = Res res } } in
        add_name_res modul.resolutions key binding
    | Foreign funcs -> List.iter (fun f -> visit_fn f modul) funcs
    | Unit name ->
        let unit_id = add_unit resolver name in
        let lib_name = "lib" ^ name ^ ".o" in
        let f name =
          let obj = Object.read_obj name in
          let metadata =
            Option.get @@ Object.read_section_by_name obj ".ray\000"
          in
          let open Metadata in
          let dec = Decoder.create metadata unit_id in
          let modul = decode_module resolver dec None in
          resolver.extern_units <- resolver.extern_units @ [modul];
          resolver.tcx.units <- resolver.tcx.units @ [name];
          decode_metadata resolver.tcx dec
        in
        let p =
          Path.join [Filename.dirname resolver.modd.mod_path; lib_name]
        in
        if Sys.file_exists p then f p
        else if Sys.file_exists (Path.join [stdlib; lib_name]) then
          f (Path.join [stdlib; lib_name])
        else (
          eprintf "error(%s): unit `%s` not found\n" resolver.modd.mod_path
            name;
          flush stderr)
    | Const _ | Import _ -> ()
  in
  List.iter visit_item resolver.modd.items;
  modul
