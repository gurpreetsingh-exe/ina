open Ast
open Ty
open Infer
open Errors
open Printf
open Session

let ty_unwrap (ty : ty option) = Option.value ty ~default:Unit

type env = { bindings: (node_id, ty) Hashtbl.t }

let env_create _ : env = { bindings = Hashtbl.create 0 }

type ty_ctx = { tcx: tcx }

type ty_err =
  | MismatchTy of ty * ty
  | UninitializedFields of ident
  | UnknownField of ident * ident
  | NoFieldInPrimitiveType of ty
  | InvalidBinaryExpression of binary_kind * ty * ty
  | MethodNotFound of ty * string

exception TypeError of ty_err

let error = ref 0

let mismatch_ty expected ty span =
  let msg =
    sprintf
      "expected `%s`, found `%s`"
      (render_ty ?dbg:(Some false) expected)
      (render_ty ?dbg:(Some false) ty)
  in
  Diagnostic.
    {
      level = Err
    ; message = "mismatch types"
    ; span = { primary_spans = [span]; labels = [span, msg, true] }
    ; children = []
    ; sugg = []
    ; loc = Diagnostic.loc __POS__
    }
;;

let invalid_binary_expr kind left right span =
  let msg =
    sprintf
      "cannot %s `%s` and `%s`"
      (match kind with
       | Add -> "add"
       | Sub -> "subtract"
       | Mul -> "multiply"
       | Div -> "divide"
       | Eq | NotEq | Gt | GtEq | Lt | LtEq -> "compare"
       | BitAnd | And -> "and"
       | BitOr | Or -> "or")
      (render_ty ?dbg:(Some false) left)
      (render_ty ?dbg:(Some false) right)
  in
  Diagnostic.
    {
      level = Err
    ; message = msg
    ; span = { primary_spans = [span]; labels = [span, msg, true] }
    ; children = []
    ; sugg = []
    ; loc = Diagnostic.loc __POS__
    }
;;

let unused_value span =
  let msg = "expression result unused" in
  Diagnostic.
    {
      level = Warn
    ; message = msg
    ; span = { primary_spans = [span]; labels = [span, "", true] }
    ; children = []
    ; sugg = []
    ; loc = Diagnostic.loc __POS__
    }
;;

let uninitialized_fields name span =
  let msg = sprintf "`%s` has uninitialized fields" name in
  Diagnostic.
    {
      level = Err
    ; message = "uninitialized fields"
    ; span = { primary_spans = [span]; labels = [span, msg, true] }
    ; children = []
    ; sugg = []
    ; loc = Diagnostic.loc __POS__
    }
;;

let unknown_field strukt name span =
  let msg = sprintf "`%s` has no field `%s`" strukt name in
  Diagnostic.
    {
      level = Err
    ; message = msg
    ; span = { primary_spans = [span]; labels = [] }
    ; children = []
    ; sugg = []
    ; loc = Diagnostic.loc __POS__
    }
;;

let no_field_in_prim_ty ty span =
  let msg =
    sprintf
      "primitive type `%s` has no fields"
      (render_ty ?dbg:(Some false) ty)
  in
  Diagnostic.
    {
      level = Err
    ; message = msg
    ; span = { primary_spans = [span]; labels = [] }
    ; children = []
    ; sugg = []
    ; loc = Diagnostic.loc __POS__
    }
;;

let method_not_found ty name span =
  let msg =
    sprintf
      "type `%s` has no method `%s`"
      (render_ty ?dbg:(Some false) ty)
      name
  in
  Diagnostic.
    {
      level = Err
    ; message = msg
    ; span = { primary_spans = [span]; labels = [] }
    ; children = []
    ; sugg = []
    ; loc = Diagnostic.loc __POS__
    }
;;

let ty_err_emit tcx ty_err span =
  match ty_err with
  | MismatchTy (expected, ty) ->
      if ty <> Err && expected <> Err
      then Sess.emit_err tcx.sess (mismatch_ty expected ty span)
  | UninitializedFields name ->
      Sess.emit_err tcx.sess (uninitialized_fields name span)
  | UnknownField (strukt, name) ->
      Sess.emit_err tcx.sess (unknown_field strukt name span)
  | NoFieldInPrimitiveType ty ->
      if ty <> Err then Sess.emit_err tcx.sess (no_field_in_prim_ty ty span)
  | InvalidBinaryExpression (kind, left, right) ->
      if left <> Err && right <> Err
      then Sess.emit_err tcx.sess (invalid_binary_expr kind left right span)
  | MethodNotFound (ty, name) ->
      if ty <> Err
      then Sess.emit_err tcx.sess (method_not_found ty name span)
;;

let ty_ctx_create (infer_ctx : infer_ctx) = { tcx = infer_ctx.tcx }

let tychk_func (ty_ctx : ty_ctx) (func : func) =
  let { fn_sig = { ret_ty; fn_span; _ }; body; _ } = func in
  let tcx = ty_ctx.tcx in
  let rec fexpr expr : ty =
    (match expr.expr_kind with
     | Binary (kind, left, right) ->
         let left, right = fexpr left, fexpr right in
         if ty_neq tcx left right
         then
           ty_err_emit
             ty_ctx.tcx
             (InvalidBinaryExpression (kind, left, right))
             expr.expr_span
     | Call (_, args) -> ignore (List.map fexpr args)
     | If { cond; then_block; else_block } ->
         if ty_neq tcx (fexpr cond) Bool then ();
         let then_ty = fblock then_block in
         (match else_block with
          | Some else_block ->
              let else_ty = fexpr else_block in
              if ty_neq tcx then_ty else_ty
              then (
                let span = expr.expr_span in
                let msg =
                  sprintf
                    "expected `%s`, found `%s`"
                    (render_ty ?dbg:(Some false) then_ty)
                    (render_ty ?dbg:(Some false) else_ty)
                in
                incr error;
                Sess.emit_err
                  tcx.sess
                  Diagnostic.
                    {
                      level = Err
                    ; message =
                        sprintf "`if` and `else` have incompatible types"
                    ; span =
                        {
                          primary_spans = [span]
                        ; labels = [span, msg, true]
                        }
                    ; children = []
                    ; sugg = []
                    ; loc = Diagnostic.loc __POS__
                    })
          | None -> ())
     | Deref expr -> ignore (fexpr expr)
     | Ref expr -> ignore (fexpr expr)
     | Block body -> ignore (fblock body)
     | StructExpr { fields; _ } ->
         let ty = Option.get expr.expr_ty in
         (match ty with
          | Struct (struct_name, tys) ->
              let strukt = Hashtbl.of_seq (List.to_seq tys) in
              if Hashtbl.length strukt <> List.length fields
              then
                ty_err_emit
                  ty_ctx.tcx
                  (UninitializedFields struct_name)
                  expr.expr_span;
              List.iter
                (fun (name, expr) ->
                  if Hashtbl.mem strukt name
                  then (
                    let ty = Hashtbl.find strukt name in
                    let t = fexpr expr in
                    if ty_neq tcx t ty
                    then
                      ty_err_emit
                        ty_ctx.tcx
                        (MismatchTy (ty, t))
                        expr.expr_span)
                  else
                    ty_err_emit
                      ty_ctx.tcx
                      (UnknownField (struct_name, name))
                      expr.expr_span)
                fields
          | Err -> () (* error was handled by infer *)
          | _ -> assert false)
     | Field (expr, name) ->
         let ty = Option.get expr.expr_ty in
         (match ty with
          | Struct (struct_name, tys) ->
              let strukt = Hashtbl.of_seq (List.to_seq tys) in
              if not (Hashtbl.mem strukt name)
              then
                ty_err_emit
                  ty_ctx.tcx
                  (UnknownField (struct_name, name))
                  expr.expr_span
          | ty ->
              ty_err_emit
                ty_ctx.tcx
                (NoFieldInPrimitiveType ty)
                expr.expr_span);
         ignore (fexpr expr)
     | Cast (expr, cast_ty) ->
         let ty = fexpr expr in
         (match ty, cast_ty with
          | RefTy t1, Ptr t2 when ty_neq tcx t1 t2 -> assert false
          | Int i1, Int i2 when size_of_int i1 = size_of_int i2 -> ()
          | RefTy _, Ptr _ | Ptr _, (Int _ | Ptr _) | (Int _ | FnTy _), Ptr _
            ->
              ()
          | t1, t2 ->
              printf "%s - %s\n" (render_ty t1) (render_ty t2);
              assert false)
     | MethodCall (e, name, args) ->
         let ty = fexpr e in
         (match lookup_assoc_fn tcx.def_table ty name with
          | Some _ -> ()
          | None ->
              ty_err_emit
                ty_ctx.tcx
                (MethodNotFound (ty, name))
                expr.expr_span);
         ignore (List.map fexpr args)
     | Lit _ | Path _ -> ());
    Option.get expr.expr_ty
  and fblock body =
    List.iter f body.block_stmts;
    match body.last_expr with Some expr -> fexpr expr | None -> Unit
  and f stmt =
    match stmt with
    | Assign (expr1, expr2) ->
        let left = fexpr expr1 in
        let right = fexpr expr2 in
        if ty_neq tcx left right
        then
          ty_err_emit ty_ctx.tcx (MismatchTy (left, right)) expr2.expr_span
    | Stmt expr | Expr expr ->
        let ty = fexpr expr in
        if ty <> Unit && ty <> Err
        then Sess.emit_err tcx.sess (unused_value expr.expr_span)
    | Binding ({ binding_pat; binding_ty; binding_expr; _ } as binding) ->
        let ty = fexpr binding_expr in
        (match binding_pat with
         | PatIdent _ ->
             (match binding_ty with
              | Some expected ->
                  binding.binding_ty <- Some (unwrap_ty ty_ctx.tcx expected);
                  if ty_neq tcx expected ty
                  then
                    ty_err_emit
                      ty_ctx.tcx
                      (MismatchTy (expected, ty))
                      binding_expr.expr_span
              | None -> binding.binding_ty <- Some ty);
             let check_overflow _value ty =
               match ty with
               | Ty.Int ty ->
                   let _ = integer_ranges ty in
                   ()
               | _ -> ()
             in
             (match binding_expr.expr_kind with
              | Lit lit ->
                  (match lit with
                   | LitInt value ->
                       check_overflow value (Option.get binding_expr.expr_ty)
                   | _ -> ())
              | _ -> ()))
    | Assert _ -> ()
  in
  let ret_ty = Option.value ret_ty ~default:Unit in
  match body with
  | Some body ->
      List.iter f body.block_stmts;
      (match body.last_expr with
       | Some expr ->
           let ty = fexpr expr in
           if ty_neq tcx ty ret_ty
           then
             ty_err_emit ty_ctx.tcx (MismatchTy (ret_ty, ty)) expr.expr_span
       | None ->
           (match ret_ty with
            | Unit -> ()
            | _ -> ty_err_emit ty_ctx.tcx (MismatchTy (ret_ty, Unit)) fn_span))
  | None -> ()
;;

let rec tychk ty_ctx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> tychk_func ty_ctx func
    | Foreign funcs -> List.iter (fun f -> tychk_func ty_ctx f) funcs
    | Impl { impl_items; _ } ->
        List.iter (function AssocFn f -> tychk_func ty_ctx f) impl_items
    | Import _ | Type _ | Unit _ -> ()
    | Mod m ->
        (match m.resolved_mod with
         | Some modd -> tychk ty_ctx modd
         | None -> ())
    | _ -> assert false
  in
  List.iter f modd.items
;;
