open Ppxlib
open Printf

let context = Extension.Context.expression
let red = "\x1b[38;2;255;60;60m"
let b = "\x1b[1m"
let l = "\x1b[2m"
let e = "\x1b[0m"

let expand ~ctxt s =
  let open Ast_builder.Default in
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let pos = loc.loc_start in
  let ice = sprintf "%s%sinternal compiler error%s" b red e in
  let src_loc = sprintf "%s[%s:%d]:%s" l pos.pos_fname pos.pos_lnum e in
  let e = sprintf "%s %s %s\n" ice src_loc s in
  let name = Ldot (Lident "Printf", "eprintf") in
  let fn = { txt = name; loc } in
  pexp_sequence
    (eapply (pexp_ident ~loc fn) [estring e ~loc] ~loc)
    (pexp_assert (ebool false ~loc) ~loc)
    ~loc
;;

let ext =
  Extension.V3.declare
    "ice"
    context
    Ast_pattern.(single_expr_payload (estring __))
    expand
;;

let rule = Context_free.Rule.extension ext
let () = Driver.V2.register_transformation ~rules:[rule] "ice"
