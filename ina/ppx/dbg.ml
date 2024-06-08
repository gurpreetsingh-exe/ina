open Ppxlib
open Printf

let context = Extension.Context.expression

let get_fmt_string (expr : expression) =
  match expr with
  | { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ } -> s
  | _ -> assert false
;;

let ident name ~loc =
  let ident = { txt = name; loc } in
  Ast_builder.Default.pexp_ident ~loc ident
;;

let l = "\x1b[2m"
let e = "\x1b[0m"

let expand ~ctxt args =
  let open Ast_builder.Default in
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let pos = loc.loc_start in
  let src_loc =
    sprintf "%s[%s:%d in %%s()]:%s" l pos.pos_fname pos.pos_lnum e
  in
  let args =
    (get_fmt_string (List.hd args) |> sprintf "%s %s" src_loc |> estring ~loc)
    :: ident (Lident "__FUNCTION__") ~loc
    :: List.tl args
  in
  let name = Ldot (Lident "Printf", "printf") in
  let fn = ident name ~loc in
  eapply fn args ~loc
;;

let expand ~ctxt _ =
  let open Ast_builder.Default in
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  eunit ~loc
;;

let ext =
  let e = Ast_pattern.pexp_tuple Ast_pattern.__ in
  Extension.V3.declare
    "dbg"
    context
    Ast_pattern.(single_expr_payload e)
    expand
;;

let rule = Context_free.Rule.extension ext
let () = Driver.V2.register_transformation ~rules:[rule] "dbg"
