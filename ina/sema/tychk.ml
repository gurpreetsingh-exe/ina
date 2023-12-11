open Ast
open Middle.Ty
open Middle.Ctx
open Errors
open Structures.Vec
open Structures.Hashmap
open Diagnostic
open Printf
open Utils.Panic
open Infer

let ( let* ) v f = Result.bind v f

type cx = {
    infcx: infer_ctx
  ; locals: (node_id, ty ref) hashmap
}

let create infcx = { infcx; locals = new hashmap }

type 'a expected_found = {
    expected: 'a
  ; found: 'a
}

type ty_err =
  | MismatchTy of ty ref * ty ref
  | UninitializedFields of ident
  | UnknownField of ident * ident
  | NoFieldInPrimitiveType of ty ref
  | InvalidBinaryExpression of binary_kind * ty ref * ty ref
  | MethodNotFound of ty ref * string
  | IntMismatch of int_ty expected_found
  | FloatMismatch of float_ty expected_found
  | InvalidDeref of ty ref
  | InvalidCall of ty ref

type 'a tyck_result = ('a, ty_err) result

type expectation =
  | NoExpectation
  | ExpectTy of ty ref

exception TypeError of ty_err

let mismatch_int_ty expected ty span =
  let msg =
    sprintf
      "expected `%s`, found `%s`"
      (display_int_ty expected)
      (display_int_ty ty)
  in
  mk_err msg span
;;

let mismatch_float_ty expected ty span =
  let msg =
    sprintf
      "expected `%s`, found `%s`"
      (display_float_ty expected)
      (display_float_ty ty)
  in
  mk_err msg span
;;

let mismatch_args expected found span =
  let msg =
    sprintf
      "expected %d arg%s, found %d"
      expected
      (if expected = 1 then "" else "s")
      found
  in
  mk_err msg span
;;

let unused_value span = mk_err "expression result unused" span

let uninitialized_fields name span =
  let msg = sprintf "`%s` has uninitialized fields" name in
  mk_err msg span
;;

let unknown_field strukt name span =
  let msg = sprintf "`%s` has no field `%s`" strukt name in
  mk_err msg span
;;

let ty_err_emit (tcx : tcx) err span =
  match err with
  | MismatchTy (expected, ty) ->
      let msg =
        sprintf
          "expected `%s`, found `%s`"
          (tcx#render_ty expected)
          (tcx#render_ty ty)
      in
      if ty <> tcx#types.err && expected <> tcx#types.err
      then tcx#emit (mk_err msg span)
  | UninitializedFields name -> tcx#emit (uninitialized_fields name span)
  | UnknownField (strukt, name) -> tcx#emit (unknown_field strukt name span)
  | NoFieldInPrimitiveType ty ->
      let msg =
        sprintf "primitive type `%s` has no fields" (tcx#render_ty ty)
      in
      if ty <> tcx#types.err then tcx#emit (mk_err msg span)
  | InvalidBinaryExpression (kind, left, right) ->
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
          (tcx#render_ty left)
          (tcx#render_ty right)
      in
      if left <> tcx#types.err && right <> tcx#types.err
      then tcx#emit (mk_err msg span)
  | MethodNotFound (ty, name) ->
      let msg =
        sprintf "type `%s` has no method `%s`" (tcx#render_ty ty) name
      in
      if ty <> tcx#types.err then tcx#emit (mk_err msg span)
  | IntMismatch { expected; found } ->
      tcx#emit (mismatch_int_ty expected found span)
  | FloatMismatch { expected; found } ->
      tcx#emit (mismatch_float_ty expected found span)
  | InvalidDeref ty ->
      let msg = sprintf "`%s` cannot be dereferenced" (tcx#render_ty ty) in
      tcx#emit (mk_err msg span)
  | InvalidCall ty ->
      let msg = sprintf "`%s` is not callable" (tcx#render_ty ty) in
      tcx#emit (mk_err msg span)
;;

let tychk_fn cx fn =
  let tcx = cx.infcx.tcx in
  let define id ty =
    ignore (tcx#node_id_to_ty#insert id ty);
    ignore (cx.locals#insert id ty)
  in
  let resolve_expected = function
    | NoExpectation -> None
    | ExpectTy ty -> Some (Infer.resolve_vars cx.infcx ty)
  in
  let write_ty id ty =
    dbg "write_ty(id = %d, ty = %s)\n" id (render_ty2 ty);
    ignore (tcx#node_id_to_ty#insert id ty)
  in
  let int_unification_error (v : IntVid.e) =
    let expected, found = v in
    IntMismatch { expected; found }
  in
  let float_unification_error (v : FloatVid.e) =
    let expected, found = v in
    FloatMismatch { expected; found }
  in
  let unify_int_var vid value =
    let* _ =
      Result.map_error
        int_unification_error
        (IntUt.unify_var_value cx.infcx.int_ut vid (Some value))
    in
    Ok (tcx#int_ty_to_ty value)
  in
  let unify_float_var vid value =
    let* _ =
      Result.map_error
        float_unification_error
        (FloatUt.unify_var_value cx.infcx.float_ut vid (Some value))
    in
    Ok (tcx#float_ty_to_ty value)
  in
  let rec equate (t0 : ty ref) (t1 : ty ref) : ty ref tyck_result =
    if t0 = t1
    then Ok t0
    else
      match !t0, !t1 with
      | Infer (IntVar i), Int t | Int t, Infer (IntVar i) ->
          unify_int_var i t
      | Infer (IntVar i0), Infer (IntVar i1) ->
          let* _ =
            Result.map_error
              int_unification_error
              (IntUt.unify_var_var cx.infcx.int_ut i0 i1)
          in
          Ok t0
      | Infer (FloatVar i), Float t | Float t, Infer (FloatVar i) ->
          unify_float_var i t
      | Infer (FloatVar i0), Infer (FloatVar i1) ->
          let* _ =
            Result.map_error
              float_unification_error
              (FloatUt.unify_var_var cx.infcx.float_ut i0 i1)
          in
          Ok t0
      | Infer _, _ | _, Infer _ -> Error (MismatchTy (t0, t1))
      | Ref t0, Ref t1 ->
          let* ty = equate t0 t1 in
          Ok (tcx#ref ty)
      | _ -> Error (MismatchTy (t0, t1))
  in
  let fold_int_ty intvid old_ty =
    let opt_ty = IntUt.probe_value cx.infcx.int_ut intvid in
    tcx#invalidate
      !old_ty
      (match opt_ty with
       | Some ty -> tcx#int_ty_to_ty ty
       | None -> tcx#types.i32)
  in
  let fold_float_ty floatvid old_ty =
    let opt_ty = FloatUt.probe_value cx.infcx.float_ut floatvid in
    tcx#invalidate
      !old_ty
      (match opt_ty with
       | Some ty -> tcx#float_ty_to_ty ty
       | None -> tcx#types.f32)
  in
  let rec check_block block =
    block.block_stmts#iter check_stmt;
    match block.last_expr with
    | Some expr -> check_expr expr NoExpectation
    | None -> tcx#types.unit
  and check_block_with_expected block expected =
    block.block_stmts#iter check_stmt;
    match block.last_expr with
    | Some expr ->
        let ty = check_expr expr expected in
        (match expected with
         | ExpectTy expected ->
             (match equate expected ty with
              | Ok _ -> ()
              | Error e -> ty_err_emit tcx e expr.expr_span);
             expected
         | NoExpectation -> ty)
    | None ->
        (match expected with
         | ExpectTy expected ->
             ty_err_emit
               tcx
               (MismatchTy (expected, tcx#types.unit))
               block.block_span;
             expected
         | NoExpectation -> tcx#types.unit)
  and check_stmt stmt =
    match stmt with
    | Assign (expr1, expr2) ->
        let left = check_expr expr1 NoExpectation in
        let right = check_expr expr2 NoExpectation in
        (match equate left right with
         | Ok _ -> ()
         | Error e -> ty_err_emit tcx e expr2.expr_span)
    | Stmt expr | Expr expr ->
        let _ = check_expr expr NoExpectation in
        ()
        (* if ty <> Unit && ty <> Err *)
        (* then tcx#emit (unused_value expr.expr_span) *)
    | Binding { binding_pat; binding_ty; binding_expr; binding_id; _ } ->
        (match binding_pat with
         | PatIdent _ ->
             (match binding_ty with
              | Some expected ->
                  let expected = tcx#ast_ty_to_ty expected in
                  let ty = check_expr binding_expr (ExpectTy expected) in
                  define binding_id expected;
                  (match equate expected ty with
                   | Ok _ -> ()
                   | Error e -> ty_err_emit tcx e binding_expr.expr_span)
              | None ->
                  let ty = check_expr binding_expr NoExpectation in
                  define binding_id ty))
    | Assert _ -> ()
  and check_expr expr expected =
    let ty = check_expr_kind expr expected in
    let ty = resolve_vars cx.infcx ty in
    write_ty expr.expr_id ty;
    ty
  and check_path path =
    tcx#res_map#unsafe_get path.path_id |> function
    | Def (id, _) -> tcx#node_id_to_ty#unsafe_get id.inner
    | Local id -> cx.locals#unsafe_get id
    | Err -> tcx#types.err
    | _ -> assert false
  and check_expr_kind expr expected =
    match expr.expr_kind with
    | Binary (kind, left, right) ->
        let left = check_expr left NoExpectation in
        let right = check_expr right NoExpectation in
        let ty =
          match equate left right with
          | Ok ty -> ty
          | Error _ ->
              let e = InvalidBinaryExpression (kind, left, right) in
              ty_err_emit tcx e expr.expr_span;
              left
        in
        (match kind with
         | Lt | LtEq | Gt | GtEq | Eq | NotEq -> tcx#types.bool
         | _ -> ty)
    | Call (expr, args) ->
        let ty = check_expr expr NoExpectation in
        (match !ty with
         | FnPtr { args = arg_tys; ret; _ } ->
             if args#len <> arg_tys#len
             then
               tcx#emit @@ mismatch_args arg_tys#len args#len expr.expr_span;
             args#iteri (fun i arg ->
                 let expected = arg_tys#get i in
                 let ty =
                   check_expr
                     arg
                     (if i < arg_tys#len
                      then ExpectTy expected
                      else NoExpectation)
                 in
                 match equate expected ty with
                 | Ok _ -> ()
                 | Error e -> ty_err_emit tcx e arg.expr_span);
             ret
         | Err -> ty
         | _ ->
             ty_err_emit tcx (InvalidCall ty) expr.expr_span;
             tcx#types.err)
    | Path path -> check_path path
    | Lit lit ->
        (match lit with
         | LitInt _ ->
             (match resolve_expected expected with
              | Some ({ contents = Int _ } as ty) -> ty
              | Some ty ->
                  let found = infcx_new_int_var cx.infcx in
                  ty_err_emit tcx (MismatchTy (ty, found)) expr.expr_span;
                  ty
              | None -> infcx_new_int_var cx.infcx)
         | LitFloat _ ->
             (match resolve_expected expected with
              | Some ({ contents = Float _ } as ty) -> ty
              | Some ty ->
                  let found = infcx_new_float_var cx.infcx in
                  ty_err_emit tcx (MismatchTy (ty, found)) expr.expr_span;
                  ty
              | None -> infcx_new_float_var cx.infcx)
         | LitStr _ -> tcx#types.str
         | LitBool _ -> tcx#types.bool)
    | Block block -> check_block_with_expected block expected
    | Deref expr ->
        let ty = check_expr expr NoExpectation in
        (match !ty with
         | Ptr ty | Ref ty -> ty
         | _ ->
             ty_err_emit tcx (InvalidDeref ty) expr.expr_span;
             tcx#types.err)
    | Ref expr ->
        (match resolve_expected expected with
         | Some expected ->
             let ty = check_expr expr NoExpectation in
             (match equate expected (tcx#ref ty) with
              | Ok _ -> ()
              | Error e -> ty_err_emit tcx e expr.expr_span);
             expected
         | None ->
             let ty = check_expr expr NoExpectation in
             tcx#ref ty)
    | If { cond; then_block; else_block; if_span; _ } ->
        ignore (check_expr cond (ExpectTy tcx#types.bool));
        let if_ty = check_block_with_expected then_block expected in
        Option.fold
          ~some:(fun expr ->
            let else_ty = check_expr expr expected in
            Result.fold
              ~ok:(fun ty -> ty)
              ~error:(fun e ->
                ty_err_emit tcx e if_span;
                else_ty)
              (equate if_ty else_ty))
          ~none:tcx#types.unit
          else_block
    | StructExpr expr ->
        let name = expr.struct_name.segments#join "::" (fun s -> s.ident) in
        let ty = check_path expr.struct_name in
        let (Variant variant) = tcx#non_enum_variant ty in
        let remaining_fields = new hashmap in
        variant.fields#iter (fun (Field { ty; name }) ->
            remaining_fields#insert' name ty);
        let check_field (ident, expr) =
          match remaining_fields#remove ident with
          | Some expected ->
              let ty = check_expr expr (ExpectTy expected) in
              (match equate expected ty with
               | Ok _ -> ()
               | Error e -> ty_err_emit tcx e expr.expr_span)
          | None ->
              let err = UnknownField (name, ident) in
              ty_err_emit tcx err expr.expr_span
        in
        expr.fields#iter check_field;
        (if remaining_fields#len <> 0
         then
           (* TODO: display all missing fields *)
           let err = UninitializedFields name in
           ty_err_emit tcx err expr.struct_expr_span);
        ty
    | Field (expr, ident) ->
        let ty = check_expr expr NoExpectation in
        let (Variant variant) = tcx#non_enum_variant ty in
        find
          (fun (Field { name; ty }) ->
            if name = ident then Some ty else None)
          variant.fields
        |> ( function
        | Some ty -> ty
        | None ->
            let path = tcx#def_id_to_qpath#unsafe_get variant.def_id in
            let name = Option.get path#last in
            let err = UnknownField (name, ident) in
            ty_err_emit tcx err expr.expr_span;
            tcx#types.err )
    | Cast _ | MethodCall _ -> assert false
  in
  let ty = tcx#node_id_to_ty#unsafe_get fn.func_id in
  let ret =
    match !ty with
    | FnPtr { ret; _ } -> ret
    | _ ->
        dbg "expected function, found `%s`\n" (tcx#render_ty ty);
        assert false
  in
  (* TODO: display better error span *)
  let span =
    match fn.fn_sig.ret_ty with
    | Some ty -> ty.span
    | None -> fn.fn_sig.fn_span
  in
  (match fn.body with
   | Some block ->
       fn.fn_sig.args#iter (fun { ty; arg_id; _ } ->
           define arg_id (tcx#ast_ty_to_ty ty));
       let ty = check_block block in
       (match equate ret ty with
        | Ok _ -> ()
        | Error e -> ty_err_emit tcx e span)
   | None -> ());
  tcx#node_id_to_ty#iter (fun _ v ->
      match !v with
      | Infer (IntVar i) -> fold_int_ty i v
      | Infer (FloatVar f) -> fold_float_ty f v
      | Infer _ -> assert false
      | _ -> ())
;;

(* tcx#node_id_to_ty#iter (fun k v -> printf "%d -> %s\n" k (render_ty !v)) *)

let rec tychk cx (modd : modd) =
  let f (item : item) =
    match item with
    | Fn (func, _) -> tychk_fn cx func
    | Foreign funcs -> funcs#iter (fun f -> tychk_fn cx f)
    | Impl { impl_items; _ } ->
        impl_items#iter (function AssocFn f -> tychk_fn cx f)
    | Type _ | Unit _ -> ()
    | Mod m ->
        (match m.resolved_mod with Some modd -> tychk cx modd | None -> ())
  in
  modd.items#iter f
;;
