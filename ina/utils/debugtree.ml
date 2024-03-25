open Ast
open Front.Ast_printer
open Printf

let print_mdl tcx (mdl : modd) =
  let indent = ref 0 in
  let enter () = incr indent in
  let leave () = decr indent in
  let tab () = String.make (!indent * 4) ' ' in
  let tab () = printf "%s" (tab ()) in
  let rec print_fn fn =
    printf "fn %s" fn.name;
    print_fn_sig fn.fn_sig;
    match fn.body with
    | Some block ->
        printf " ";
        print_block block
    | _ -> printf ";\n"
  and print_fn_sig sign =
    let arglen = sign.args#len in
    printf "(";
    sign.args#iteri (fun i { arg; ty; _ } ->
        printf "%s: " arg;
        print_ty ty;
        if i < arglen - 1 then printf ", ");
    match sign.ret_ty with
    | Some ty ->
        printf ") -> ";
        print_ty ty
    | _ -> printf ")"
  and print_block block =
    enter ();
    printf "{\n";
    block.block_stmts#iter print_stmt;
    leave ();
    printf "}"
  and print_stmt = function
    | Stmt e ->
        tab ();
        print_expr e;
        printf ";\n"
    | Expr e ->
        tab ();
        print_expr e
    | Binding { binding_pat; binding_ty; binding_expr; _ } ->
        tab ();
        printf "let ";
        print_pat binding_pat;
        (match binding_ty with
         | Some ty ->
             printf ": ";
             print_ty ty
         | None -> ());
        printf " = ";
        print_expr binding_expr;
        printf ";\n"
    | Assign (left, right) ->
        tab ();
        print_expr left;
        printf " = ";
        print_expr right
    | Assert _ -> assert false
  and print_mut mut = printf "%s" (if mut = Mut then "mut " else "")
  and print_pat = function
    | PIdent (mut, ident) ->
        print_mut mut;
        printf "%s" ident
    | _ -> assert false
  and print_expr expr =
    let ty = tcx#get_def_debug expr.expr_id in
    enter ();
    printf "{\n";
    tab ();
    (match expr.expr_kind with
     | Call (expr, args) ->
         print_expr expr;
         printf "(";
         args#iter print_expr;
         printf ")"
     | Path path ->
         let name = path.segments#join "::" (fun s -> s.ident) in
         printf "%s" name
     | Lit lit ->
         (match lit with
          | LitInt i -> print_int i
          | LitFloat f -> print_float f
          | LitBool b -> printf "%b" b
          | LitStr s -> print_string s)
     | Ref (mut, expr) ->
         printf "&";
         print_mut mut;
         print_expr expr
     | Cast (expr, ty) ->
         print_expr expr;
         printf " as ";
         print_ty ty
     | _ -> assert false);
    leave ();
    printf ": %s\n" (tcx#render_ty ty);
    tab ();
    printf "}"
  and print_ty ty = printf "%s" (render_ty ty) in
  mdl.items#iter (function Fn (fn, _) -> print_fn fn | _ -> ());
  print_newline ();
  exit 0
;;
