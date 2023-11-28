open Ast
open Context

let lower_block (lcx : lcx) block =
  let tcx = lcx#tcx in
  let bx = lcx#builder in
  let rec lower_block' () =
    let f stmt =
      match stmt with
      | Binding binding ->
          let { binding_expr; binding_id; _ } = binding in
          let ty = tcx#node_id_to_ty#unsafe_get binding_id in
          let ptr = bx#alloca ty in
          assert (lcx#locals#insert binding_id ptr = None);
          let src = lower binding_expr in
          bx#store src ptr
      | _ -> ()
    in
    block.block_stmts#iter f;
    match block.last_expr with Some expr -> lower expr | None -> bx#nop
  and lower_lit lit ty =
    match lit with
    | LitInt value -> bx#const_int ty value
    | LitFloat value -> bx#const_float ty value
    | LitStr value -> bx#const_string ty value
    | LitBool value -> bx#const_bool ty value
  and lower expr =
    let ty = tcx#node_id_to_ty#unsafe_get expr.expr_id in
    match expr.expr_kind with
    | Lit lit -> lower_lit lit ty
    | Path path ->
        let res = tcx#res_map#unsafe_get path.path_id in
        (match res with
         | Local id ->
             let ptr = lcx#locals#unsafe_get id in
             bx#load ptr
         | _ -> assert false)
    | _ -> assert false
  in
  lower_block' ()
;;
