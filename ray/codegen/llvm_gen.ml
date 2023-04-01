open Ast
open Llvm

let ctx = global_context ()

let gen_function_type (_ : fn_sig) : lltype =
  function_type (i32_type ctx) [||]

let gen_func (func : func) (ll_mod : llmodule) =
  let function_type = gen_function_type func.fn_sig in
  if func.is_extern then assert false
  else define_function func.fn_sig.name function_type ll_mod

let gen_item (item : item) (ll_mod : llmodule) =
  match item with Fn (func, _) -> gen_func func ll_mod | _ -> assert false

let gen_module (name : string) (modd : modd) =
  let ll_mod = create_module ctx name in
  ignore (List.map (fun item -> gen_item item ll_mod) modd.items);
  dump_module ll_mod
