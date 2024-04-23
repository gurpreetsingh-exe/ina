open Ast
open Middle.Ty
open Middle.Ctx
open Middle.Def_id
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

let use_of_moved_value name prev_span =
  Diagnostic.create
    (sprintf "use of moved value `%s`" name)
    ~labels:[Label.secondary "value moved here" prev_span]
;;

let analyze (tcx : tcx) fn =
  let locals = Hashtbl.create 0 in
  let moved = Hashtbl.create 0 in
  let rec visit_block ?(e = `None) block =
    block.block_stmts#iter (fun stmt ->
        match visit_stmt ~e stmt with
        | Ok _ -> ()
        | Error (_, e) -> tcx#emit e);
    match block.last_expr with
    | Some expr -> visit_expr expr ~e
    | None -> Ok ()
  and visit_block_with_exp ?(e = `None) block =
    block.block_stmts#iter (fun stmt ->
        match visit_stmt ~e stmt with
        | Ok _ -> ()
        | Error (_, e) -> tcx#emit e);
    match block.last_expr with
    | Some expr -> visit_expr expr ~e
    | None -> Ok ()
  and visit_expr ?(e = `None) expr =
    let ty expr = tcx#get_def (local_def_id expr.expr_id) in
    let expr_name = function
      | Path path -> Some (path.segments#join "" (fun s -> s.ident))
      | _ -> None
    in
    match expr.expr_kind with
    | Lit _ -> Ok ()
    | Path path ->
        let check_path ?(move = true) path ty =
          match tcx#res_map#unsafe_get path.path_id with
          | Local (_, id) when Hashtbl.mem moved id ->
              let name = path.segments#join "" (fun s -> s.ident) in
              let prev_use = Hashtbl.find moved id in
              let err =
                use_of_moved_value name prev_use
                |> Diagnostic.label
                     (Label.primary "value used here after move" path.span)
              in
              Error (name, err)
          | Local (_, id) ->
              Ok
                (if (not (tcx#is_copy ty)) && move
                 then Hashtbl.add moved id path.span)
          | _ -> Ok ()
        in
        (match e with
         | `Deref -> Ok ()
         | `Field -> check_path path (ty expr) ~move:false
         | `Method segment ->
             let ty = ty expr in
             let ty = tcx#lookup_method ty segment.ident in
             let self = (Fn.args tcx ty)#get 0 in
             check_path path self
         | `Branch -> check_path path (ty expr) ~move:false
         | `None -> check_path path (ty expr)
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
              | Def (_, Cons) -> Ok ()
              | _ -> assert false))
    | Call (_, args) ->
        args#iter (fun expr -> ignore (visit_expr expr));
        Ok ()
    | Binary (_, left, right) ->
        let* _ = visit_expr left ~e in
        let* _ = visit_expr right ~e in
        Ok ()
    | If { cond; then_block; else_block; _ } ->
        let* _ = visit_expr cond ~e in
        let* _ = visit_block_with_exp then_block in
        let* _ =
          Option.map (fun expr -> visit_expr expr ~e) else_block
          |> Option.value ~default:(Ok ())
        in
        Ok ()
    | Block block -> visit_block_with_exp block ~e
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
        fields#iter (fun (_, expr) -> ignore (visit_expr expr ~e));
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
                        "cannot assign to `*%s`, which is behind %s"
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
               msg
               ~labels:[Label.primary "cannot assign to this" expr.expr_span]
             |> tcx#emit
         | (`None | `Method _)
           when tcx#is_ref ty
                && not (tcx#is_copy (tcx#inner_ty ty |> Option.get)) ->
             let msg =
               expr_name expr'.expr_kind
               |> Option.map (fun name ->
                      sprintf
                        "cannot move out of `*%s`, which is behind %s"
                        name
                        (tcx#describe_pointer ty))
               |> Option.value
                    ~default:
                      (sprintf
                         "cannot move out of this expression, which is \
                          behind %s"
                         (tcx#describe_pointer ty))
             in
             let ty = tcx#inner_ty ty |> Option.get in
             let pmsg =
               expr_name expr'.expr_kind
               |> Option.map (fun name ->
                      sprintf
                        "move occurs because `*%s` has type `%s`"
                        name
                        (tcx#render_ty ty))
               |> Option.value
                    ~default:
                      (sprintf
                         "move occurs because this has type `%s`"
                         (tcx#render_ty ty))
             in
             Diagnostic.create
               msg
               ~labels:[Label.primary pmsg expr.expr_span]
             |> tcx#emit
         | _ -> ());
        visit_expr expr' ~e:`Deref
    | Field (expr, _) -> visit_expr expr ~e:`Field
    | Cast (expr, _) -> visit_expr expr ~e
    | MethodCall (expr, segment, args) ->
        let* _ = visit_expr expr ~e:(`Method segment) in
        args#iter (fun expr -> ignore (visit_expr expr ~e));
        Ok ()
    | Match (expr, arms) ->
        let* _ = visit_expr expr ~e in
        let did = local_def_id expr.expr_id in
        let ty = tcx#get_def did in
        let decision = Exhaustiveness.go tcx ty arms expr.expr_span in
        tcx#record_decision_tree did decision;
        arms#iter (fun { expr; _ } ->
            match visit_expr expr ~e:`Branch with
            | Ok () -> ()
            | Error (_, e) -> tcx#emit e);
        Ok ()
  and visit_stmt ?(e = `None) = function
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
        visit_expr right
    | Stmt expr | Expr expr -> visit_expr expr ~e
    | Binding { binding_pat; binding_span; binding_expr; binding_id; _ } ->
        let* _ = visit_expr binding_expr in
        let ty = tcx#get_def (local_def_id binding_expr.expr_id) in
        let rec go span = function
          | PIdent (_, _, id) -> Hashtbl.add locals id span
          | PCons (_, patns) -> patns#iter @@ go span
          | PWild | PPath _ | PInt _ | PBool _ -> ()
        in
        go binding_span binding_pat;
        let decision =
          Exhaustiveness.check_let tcx ty binding_pat binding_span
        in
        tcx#record_decision_tree (local_def_id binding_id) decision;
        Ok ()
    | Assert (expr, _) -> visit_expr expr
  in
  match fn.body with
  | Some block ->
      (match visit_block block with Ok _ -> () | Error (_, e) -> tcx#emit e)
  | None -> ()
;;

let rec analyze_module tcx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> analyze tcx func
    | Foreign (funcs, _) -> funcs#iter (fun f -> analyze tcx f)
    | Impl { items; _ } -> items#iter (function AssocFn f -> analyze tcx f)
    | Type _ | ExternMod _ | Using _ -> ()
    | Mod m ->
        (match m.resolved_mod with
         | Some modd -> analyze_module tcx modd
         | None -> ())
  in
  modd.items#iter f
;;
