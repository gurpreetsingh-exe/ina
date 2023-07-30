open Printf
open Ast
open Resolver
open Ty

let rec resolve_path (modul : modul) (path : path) : res =
  let rec resolve_path' segs modul : res =
    let ident = List.hd segs
    and ns = if List.length segs = 1 then Value else Type in
    let key = { ident; ns } in
    match get_name_res modul.resolutions key with
    | Some res -> (
      match res.binding with
      | Some binding -> (
        match binding.kind with
        | Res res -> res
        | Module modul -> resolve_path' (List.tl segs) modul)
      | None -> Err)
    | None -> (
      match modul.parent with
      | Some modul' -> (
        match modul.mkind with Block -> resolve_path modul' path | _ -> Err)
      | _ -> Err)
  in
  let res = resolve_path' path.segments modul in
  res

let rec resolve_paths (resolver : Resolver.t) (modul : modul) (modd : modd) :
    unit =
  let resolve_ty _ty = () in
  let rec visit_expr (modul : modul) expr =
    match expr.expr_kind with
    | Call (path, exprs) ->
        List.iter (fun expr -> visit_expr modul expr) exprs;
        path.res <- resolve_path modul path
    | Binary (_, left, right) ->
        visit_expr modul left; visit_expr modul right
    | Path path -> path.res <- resolve_path modul path
    | Lit _ -> ()
    | _ ->
        print_endline (Front.Fmt.render_expr expr 0);
        assert false
  in
  let visit_body (modul : modul) body =
    List.iter
      (fun stmt ->
        match stmt with
        | Stmt expr | Expr expr -> visit_expr modul expr
        | Binding { binding_expr = expr; _ } -> visit_expr modul expr
        | _ -> assert false)
      body.block_stmts;
    match body.last_expr with
    | Some expr -> visit_expr modul expr
    | None -> ()
  in
  let visit_item (item : item) =
    match item with
    | Mod { resolved_mod; _ } -> (
      match resolved_mod with
      | Some modd -> (
          let key = { ident = modd.mod_name; ns = Type } in
          let res =
            (Option.get
               (Option.get (get_name_res modul.resolutions key)).binding)
              .kind
          in
          match res with
          | Res _ -> ()
          | Module modul -> resolve_paths resolver modul modd)
      | None -> ())
    | Fn (func, _) -> (
        let key = (Option.get func.func_path).res in
        let binding = Hashtbl.find resolver.scope_table key in
        let m = Res.modul binding in
        List.iter (fun (ty, _, _) -> resolve_ty ty) func.fn_sig.args;
        match func.body with Some body -> visit_body m body | None -> ())
    | Type (Struct _) -> ()
    | Foreign _ -> ()
    | Const _ | Import _ -> ()
  in
  List.iter visit_item modd.items
