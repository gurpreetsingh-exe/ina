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

let resolve_path resolver (modul : modul) (path : path) ns : res =
  let segs = path.segments in
  let segs_len = List.length segs in
  let ns = Option.value ~default:Type ns in
  match (ns, segs_len) with
  | Value, 1 ->
      resolve_ident_in_lexical_scope resolver modul (List.hd segs) Value
  | _ ->
      let modul = ref (get_root_mod resolver modul) in
      let res = ref Err in
      for i = 0 to segs_len - 1 do
        let ns = if i = segs_len - 1 then ns else Type in
        let ident = List.nth segs i in
        let key = { ident; ns; disambiguator = 0 } in
        match get_name_res !modul.resolutions key with
        | Some r -> (
          match r.binding.kind with
          | Res r -> res := r
          | Module modul' -> modul := modul')
        | None -> ()
      done;
      !res

let rec resolve_paths (resolver : Resolver.t) (modul : modul) (modd : modd) :
    unit =
  let resolve_ty _ty = () in
  let rec visit_expr expr (modul : modul) =
    match expr.expr_kind with
    | Call (path, exprs) ->
        path.res <- resolve_path resolver modul path (Some Value);
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
    | _ ->
        print_endline (Front.Fmt.render_expr expr 0);
        assert false
  and visit_block body (modul : modul) =
    Disambiguator.push resolver.disambiguator;
    List.iter
      (fun stmt ->
        match stmt with
        | Stmt expr | Expr expr -> visit_expr expr modul
        | Binding { binding_pat; binding_id; binding_expr = expr; _ } -> (
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
    | Fn (func, _) -> (
        let key =
          { ident = func.fn_sig.name; ns = Value; disambiguator = 0 }
        in
        resolver.key <- key;
        let m = find_scope_res modul.scope_table key in
        List.iter
          (fun (_, name, id) ->
            let key = { ident = name; ns = Value; disambiguator = 0 } in
            let binding = { binding = { kind = Res (Local id) } } in
            add_name_res m.resolutions key binding)
          func.fn_sig.args;
        List.iter (fun (ty, _, _) -> resolve_ty ty) func.fn_sig.args;
        match func.body with Some body -> visit_block body m | None -> ())
    | Type (Struct _) -> ()
    | Foreign _ -> ()
    | Const _ | Import _ -> ()
  in
  List.iter visit_item modd.items
