open Ast
open Resolver
open Ty

let resolve_path (resolutions : resolutions) (path : path) : unit =
  let rec resolve_path' segs resolutions : res =
    let ident = List.hd segs
    and ns = if List.length segs = 1 then Value else Type in
    let key = { ident; ns } in
    match get_name_res resolutions key with
    | Some res -> (
      match res.binding with
      | Some binding -> (
        match binding.kind with
        | Res res -> res
        | Module modul -> resolve_path' (List.tl segs) modul.resolutions)
      | None -> Err)
    | None -> Local
  in
  path.res <- resolve_path' path.segments resolutions

let rec resolve_paths (resolver : Resolver.t) (resolutions : resolutions)
    (modd : modd) : unit =
  let resolve_ty ty = () in
  let rec resolve_expr expr =
    match expr.expr_kind with
    | Call (path, exprs) ->
        List.iter resolve_expr exprs;
        resolve_path resolutions path
    | Binary (_, left, right) -> resolve_expr left; resolve_expr right
    | Path path -> resolve_path resolutions path
    | Lit _ -> ()
    | _ ->
        print_endline (Front.Fmt.render_expr expr 0);
        assert false
  in
  let resolve_body body =
    List.iter
      (fun stmt ->
        match stmt with
        | Stmt expr | Expr expr -> resolve_expr expr
        | _ -> assert false)
      body.block_stmts;
    match body.last_expr with Some expr -> resolve_expr expr | None -> ()
  in
  let visit_item (item : item) =
    match item with
    | Mod { resolved_mod; _ } -> (
      match resolved_mod with
      | Some modd -> resolve_paths resolver resolutions modd
      | None -> ())
    | Fn (func, _) -> (
        List.iter (fun (ty, name) -> resolve_ty ty) func.fn_sig.args;
        match func.body with Some body -> resolve_body body | None -> ())
    | Type (Struct s) -> ()
    | Foreign _ -> ()
    | Const _ | Import _ -> ()
  in
  List.iter visit_item modd.items
