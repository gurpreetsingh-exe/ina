open Printf
open Ast
open Resolver
open Ty

let rec resolve_path resolver (modul : modul) (path : path) : res =
  let rec resolve_path' segs modul : res =
    let ident = List.hd segs
    and ns = if List.length segs = 1 then Value else Type in
    let key = { ident; ns; disambiguator = 0 } in
    (* TODO: handle recursive paths *)
    match get_name_res modul.resolutions key with
    | Some res -> (
      match res.binding.kind with
      | Res res -> res
      | Module modul' -> (
        match segs with
        | [_] -> Err
        | _ -> resolve_path' (List.tl segs) modul'))
    | None -> (
      match modul.parent with
      | Some modul' -> (
        match modul.mkind with
        | Block -> resolve_path resolver modul' path
        | _ -> Err)
      | _ -> Err)
  in
  let res = resolve_path' path.segments modul in
  res

let rec resolve_paths (resolver : Resolver.t) (modul : modul) (modd : modd) :
    unit =
  let resolve_ty _ty = () in
  let rec visit_expr expr (modul : modul) =
    match expr.expr_kind with
    | Call (path, exprs) ->
        List.iter (fun expr -> visit_expr expr modul) exprs;
        path.res <- resolve_path resolver modul path
    | Binary (_, left, right) ->
        visit_expr left modul; visit_expr right modul
    | Path path -> path.res <- resolve_path resolver modul path
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
                visit_expr expr modul;
                let key = { ident; ns = Value; disambiguator = 0 } in
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
      | Some modd -> (
          let key =
            { ident = modd.mod_name; ns = Type; disambiguator = 0 }
          in
          let res =
            (Option.get (get_name_res modul.resolutions key)).binding.kind
          in
          match res with
          | Res _ -> ()
          | Module modul -> resolve_paths resolver modul modd)
      | None -> ())
    | Fn (func, _) -> (
        let key =
          { ident = func.fn_sig.name; ns = Value; disambiguator = 0 }
        in
        resolver.key <- key;
        let m = find_scope_res resolver key in
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
