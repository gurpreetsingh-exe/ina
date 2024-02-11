open Ast
open Middle.Ty
open Middle.Ctx
open Errors
open Diagnostic
open Printf

let assign_to_imm_var name defspan =
  let msg = sprintf "`%s` defined here" name in
  Diagnostic.create
    "assignment to immutable variable"
    ~labels:[Label.secondary msg defspan]
;;

let mut_borrow_of_imm_var name defspan =
  let msg = sprintf "`%s` defined here" name in
  Diagnostic.create
    "mutable borrow of immutable variable"
    ~labels:[Label.secondary msg defspan]
;;

let analyze (tcx : tcx) fn =
  let locals = Hashtbl.create 0 in
  let rec visit_block block =
    block.block_stmts#iter visit_stmt;
    match block.last_expr with
    | Some expr -> ignore (visit_expr expr)
    | None -> ()
  and visit_block_with_exp block =
    block.block_stmts#iter visit_stmt;
    match block.last_expr with Some expr -> visit_expr expr | None -> Ok ()
  and visit_expr ?(e = `None) expr =
    let ty expr = tcx#get_def (Middle.Def_id.local_def_id expr.expr_id) in
    let expr_name = function
      | Path path -> Some (path.segments#join "" (fun s -> s.ident))
      | _ -> None
    in
    match expr.expr_kind with
    | Lit _ -> Ok ()
    | Path path ->
        (match e with
         | `None -> Ok ()
         | `Assign ->
             (match tcx#res_map#unsafe_get path.path_id with
              | Local (mut, id) when mut = Imm ->
                  let span = Hashtbl.find locals id in
                  let name = path.segments#join "" (fun s -> s.ident) in
                  Error (name, assign_to_imm_var name span)
              | Local _ -> Ok ()
              | _ -> assert false)
         | `Ref m ->
             (match tcx#res_map#unsafe_get path.path_id with
              | Local (mut, id) ->
                  let span = Hashtbl.find locals id in
                  let name = path.segments#join "" (fun s -> s.ident) in
                  (match mut, m with
                   | Imm, Mut -> Error (name, mut_borrow_of_imm_var name span)
                   | _ -> Ok ())
              | _ -> assert false))
    | Call (_, args) ->
        args#iter (fun expr -> ignore (visit_expr expr));
        Ok ()
    | Binary (_, left, right) ->
        let* _ = visit_expr left in
        let* _ = visit_expr right in
        Ok ()
    | If { cond; then_block; else_block; _ } ->
        let* _ = visit_expr cond in
        let* _ = visit_block_with_exp then_block in
        let* _ =
          Option.map (fun expr -> visit_expr expr) else_block
          |> Option.value ~default:(Ok ())
        in
        Ok ()
    | Block block -> visit_block_with_exp block
    | Ref (m, expr') ->
        let m = tcx#ast_mut_to_mut m in
        (match visit_expr expr' ~e:(`Ref m) with
         | Ok () -> ()
         | Error (name, e) ->
             let msg =
               sprintf
                 "cannot borrow `%s` as mutable, as it is not declared as \
                  mutable"
                 name
             in
             e
             |> Diagnostic.label (Label.primary msg expr.expr_span)
             |> tcx#emit);
        Ok ()
    | StructExpr { fields; _ } ->
        fields#iter (fun (_, expr) -> ignore (visit_expr expr));
        Ok ()
    | Deref expr' ->
        let ty = ty expr' in
        (match e with
         | `Assign when tcx#is_mut_ptr ty -> ()
         | `Assign ->
             let msg =
               expr_name expr'.expr_kind
               |> Option.map (fun name ->
                      sprintf
                        "cannot assign to `%s`, which is behind %s"
                        name
                        (tcx#describe_pointer ty))
               |> Option.value
                    ~default:
                      (sprintf
                         "cannot assign to this expression, which is behind \
                          %s"
                         (tcx#describe_pointer ty))
             in
             Diagnostic.create
               "invalid assignment"
               ~labels:[Label.primary msg expr.expr_span]
             |> tcx#emit
         | _ -> ());
        visit_expr expr'
    | Field (expr, _) -> visit_expr expr
    | Cast (expr, _) -> visit_expr expr
    | MethodCall (expr, _, args) ->
        let* _ = visit_expr expr in
        args#iter (fun expr -> ignore (visit_expr expr));
        Ok ()
  and visit_stmt = function
    | Assign (left, right) ->
        (match visit_expr left ~e:`Assign with
         | Ok () -> ()
         | Error (name, e) ->
             let msg =
               sprintf "cannot assign to immutable variable `%s`" name
             in
             e
             |> Diagnostic.label (Label.primary msg left.expr_span)
             |> tcx#emit);
        ignore (visit_expr right)
    | Stmt expr | Expr expr -> ignore @@ visit_expr expr
    | Binding { binding_id; binding_span; binding_expr; _ } ->
        ignore @@ visit_expr binding_expr;
        Hashtbl.add locals binding_id binding_span
    | Assert (expr, _) -> ignore @@ visit_expr expr
  in
  match fn.body with Some block -> visit_block block | None -> ()
;;

let rec analyze_module tcx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> analyze tcx func
    | Foreign (funcs, _) -> funcs#iter (fun f -> analyze tcx f)
    | Impl { items; _ } -> items#iter (function AssocFn f -> analyze tcx f)
    | Type _ | ExternMod _ -> ()
    | Mod m ->
        (match m.resolved_mod with
         | Some modd -> analyze_module tcx modd
         | None -> ())
  in
  modd.items#iter f
;;
