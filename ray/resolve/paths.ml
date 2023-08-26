open Printf
open Ast
open Resolver
open Ty

let rec get_root_mod resolver modul : modul =
  match modul.mkind with
  | Def (kind, id, _) ->
      assert (kind = Mod);
      Hashtbl.find resolver.mod_table id
  | Block -> (
    match modul.parent with
    | Some m -> get_root_mod resolver m
    | _ -> assert false)

let rec resolve_ident_in_lexical_scope resolver modul ident ns =
  let key = { ident; ns; disambiguator = 0 } in
  match get_name_res modul.resolutions key with
  | Some r -> (
    match r.binding.kind with
    | Res r -> r
    | Module modul ->
        (* recursive function *)
        resolve_ident_in_lexical_scope resolver
          (get_root_mod resolver modul)
          ident ns)
  | None -> (
    match modul.parent with
    | Some modul' -> (
      match modul.mkind with
      | Block -> resolve_ident_in_lexical_scope resolver modul' ident ns
      | _ -> Err)
    | _ -> Err)

let resolve_path_in_modul modul path ns =
  let segs_len = List.length path in
  let res = ref Err in
  let unit_in_path_report = ref false in
  for i = 0 to segs_len - 1 do
    let ns = if i = segs_len - 1 then ns else Type in
    let ident = List.nth path i in
    if i <> 0 && ident = "unit" && not !unit_in_path_report then (
      unit_in_path_report := true;
      eprintf "error: `unit` can only appear at the start of a path\n";
      flush stdout)
    else (
      let key = { ident; ns; disambiguator = 0 } in
      match get_name_res !modul.resolutions key with
      | Some r -> (
        match r.binding.kind with
        | Res r -> res := r
        | Module modul' -> modul := modul')
      | None -> ())
  done;
  !res

let resolve_path resolver (modul : modul) (path : path) ns : res =
  let segs = path.segments in
  let segs_len = List.length segs in
  let ns = Option.value ~default:Type ns in
  match (ns, segs_len, List.hd segs) with
  | Value, 1, "unit" -> assert false
  | Value, 1, _ ->
      resolve_ident_in_lexical_scope resolver modul (List.hd segs) Value
  | _, _, "unit" ->
      let modul = ref resolver.unit_root in
      resolve_path_in_modul modul (List.tl segs) ns
  | _ ->
      let modul = ref (get_root_mod resolver modul) in
      resolve_path_in_modul modul segs ns

let resolve_path resolver modul path ns =
  let res = ref @@ resolve_path resolver modul path ns in
  (match !res with
  | Err ->
      let i = ref 0 in
      while !i < List.length resolver.extern_units && !res = Err do
        let modul = List.nth resolver.extern_units !i in
        (* TODO: not gonna work when `using` is introduced *)
        let name =
          match modul.mkind with
          | Def (_, _, name) -> name
          | Block -> assert false
        in
        if List.hd path.segments = name then
          res := resolve_path resolver modul path ns;
        incr i
      done
  | _ -> ());
  !res

let rec resolve_paths (resolver : Resolver.t) (modul : modul) (modd : modd) :
    unit =
  let rec resolve_ty ty =
    match ty with
    | Struct (_, fields) -> List.iter (fun (_, ty) -> resolve_ty ty) fields
    | FnTy (args, ty, _) ->
        List.iter (fun ty -> resolve_ty ty) args;
        resolve_ty ty
    | Ident path -> path.res <- resolve_path resolver modul path (Some Type)
    | RefTy ty | Ptr ty -> resolve_ty ty
    | Int _ | Float _ | Bool | Str | Unit -> ()
    | _ ->
        print_endline (render_ty ty);
        assert false
  in
  let rec visit_expr expr (modul : modul) =
    match expr.expr_kind with
    | Call (path, exprs) ->
        let res = resolve_path resolver modul path (Some Value) in
        let name = List.nth path.segments (List.length path.segments - 1) in
        path.res <-
          (match res with
          | Def (id, Struct) ->
              let ty =
                lookup_def resolver.tcx id |> function Ty ty -> ty
              in
              Def (lookup_assoc_fn resolver.tcx.def_table ty name, Fn)
          | _ -> res);
        List.iter (fun expr -> visit_expr expr modul) exprs
    | Binary (_, left, right) ->
        visit_expr left modul; visit_expr right modul
    | Path path -> path.res <- resolve_path resolver modul path (Some Value)
    | If { cond; then_block; else_block } -> (
        visit_expr cond modul;
        let key = key resolver in
        Disambiguator.inc resolver.disambiguator;
        let nb = Option.get (get_name_res modul.resolutions key) in
        let m = Res.modul nb.binding in
        visit_block then_block m;
        match else_block with
        | Some expr -> visit_expr expr modul
        | None -> ())
    | Ref expr | Deref expr -> visit_expr expr modul
    | Block block ->
        let key = key resolver in
        Disambiguator.inc resolver.disambiguator;
        let nb = Option.get (get_name_res modul.resolutions key) in
        let modul = Res.modul nb.binding in
        visit_block block modul
    | Lit _ -> ()
    | StructExpr { struct_name; fields } ->
        struct_name.res <-
          resolve_path resolver modul struct_name (Some Type);
        List.iter (fun (_, expr) -> visit_expr expr modul) fields
    | Field (expr, _) -> visit_expr expr modul
    | Cast (expr, ty) -> visit_expr expr modul; resolve_ty ty
  and visit_block body (modul : modul) =
    Disambiguator.push resolver.disambiguator;
    List.iter
      (fun stmt ->
        match stmt with
        | Stmt expr | Expr expr -> visit_expr expr modul
        | Binding
            { binding_pat; binding_id; binding_expr = expr; binding_ty } -> (
            (match binding_ty with Some ty -> resolve_ty ty | None -> ());
            binding_pat
            |> function
            | PatIdent ident ->
                let key = { ident; ns = Value; disambiguator = 0 } in
                visit_expr expr modul;
                let binding =
                  { binding = { kind = Res (Local binding_id) } }
                in
                add_name_res modul.resolutions key binding)
        | Assert (expr, _) -> visit_expr expr modul
        | Assign (expr1, expr2) ->
            visit_expr expr1 modul; visit_expr expr2 modul)
      body.block_stmts;
    (match body.last_expr with
    | Some expr -> visit_expr expr modul
    | None -> ());
    Disambiguator.pop resolver.disambiguator
  in
  let visit_fn (func : func) (modul : modul) =
    let key = { ident = func.fn_sig.name; ns = Value; disambiguator = 0 } in
    resolver.key <- key;
    let id = def_id func.func_id 0 in
    (lookup_def resolver.tcx id |> function Ty ty -> resolve_ty ty);
    List.iter (fun (ty, _, _) -> resolve_ty ty) func.fn_sig.args;
    match func.body with
    | Some body ->
        let m = find_scope_res modul.scope_table key in
        List.iter
          (fun (_, name, id) ->
            let key = { ident = name; ns = Value; disambiguator = 0 } in
            let binding = { binding = { kind = Res (Local id) } } in
            add_name_res m.resolutions key binding)
          func.fn_sig.args;
        visit_block body m
    | None -> ()
  in
  let visit_item (item : item) =
    match item with
    | Mod { resolved_mod; _ } -> (
      match resolved_mod with
      | Some modd ->
          let key =
            { ident = modd.mod_name; ns = Type; disambiguator = 0 }
          in
          resolver.key <- key;
          let res =
            (Option.get (get_name_res modul.resolutions key)).binding
          in
          let modul = Res.modul res in
          resolve_paths resolver modul modd
      | None -> ())
    | Fn (func, _) -> visit_fn func modul
    | Type (Struct s) -> (
        let id = def_id s.struct_id 0 in
        lookup_def resolver.tcx id |> function Ty ty -> resolve_ty ty)
    | Foreign fns -> List.iter (fun f -> visit_fn f modul) fns
    | Impl { impl_ty; impl_items } ->
        let ident =
          match impl_ty with
          | Ident path ->
              List.nth path.segments (List.length path.segments - 1)
          | _ -> render_ty impl_ty
        in
        let key = { ident; ns = Type; disambiguator = 1 } in
        let res =
          (Option.get (get_name_res modul.resolutions key)).binding
        in
        let modul = Res.modul res in
        let tbl = Hashtbl.find resolver.tcx.def_table.impls impl_ty in
        Hashtbl.remove resolver.tcx.def_table.impls impl_ty;
        resolve_ty impl_ty;
        let impl_ty = unwrap_ty resolver.tcx impl_ty in
        Hashtbl.add resolver.tcx.def_table.impls impl_ty @@ Hashtbl.copy tbl;
        List.iter (function AssocFn fn -> visit_fn fn modul) impl_items
    | Unit _ | Const _ | Import _ -> ()
  in
  List.iter visit_item modd.items
