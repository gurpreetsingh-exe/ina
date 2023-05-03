open Ast
open Llvm
open Llvm_target
open Llvm_X86

let ctx = global_context ()

(* we define our own *)
external x86AsmPrinterInit : unit -> unit = "LLVMInitializeX86AsmPrinter"

type env = {
  mutable bindings : (string, llvalue) Hashtbl.t;
  parent : env option;
}

let scope = ref { bindings = Hashtbl.create 0; parent = None }

let rec find_val scope ident =
  if Hashtbl.mem scope.bindings ident then Hashtbl.find scope.bindings ident
  else (
    match scope.parent with
    | Some s -> find_val s ident
    | None -> assert false)

let get_llvm_ty (ty : ty) : lltype =
  (match ty with
  | Prim ty -> (
    match ty with
    | I64 -> i64_type
    | I32 -> i32_type
    | F32 -> float_type
    | Bool -> i1_type)
  | Unit -> void_type
  | _ -> assert false)
    ctx

let gen_function_type (fn_sig : fn_sig) : lltype =
  let args = List.map (fun (ty, _) -> get_llvm_ty ty) fn_sig.args in
  let ret_ty = get_llvm_ty (Option.value fn_sig.ret_ty ~default:Unit) in
  function_type ret_ty (Array.of_list args)

let gen_expr (builder : llbuilder) (expr : expr) : llvalue =
  let ty = get_llvm_ty (Option.get expr.expr_ty) in
  match expr.expr_kind with
  | Lit lit -> (
    match lit with
    | LitInt value -> const_int ty value
    | LitBool value -> const_int ty (if value then 1 else 0))
  | Ident ident ->
      let ptr = find_val !scope ident in
      build_load ty ptr "" builder

let gen_block (builder : llbuilder) (block : block) =
  let f stmt =
    match stmt with
    | Binding { binding_pat; binding_ty; binding_expr; _ } -> (
      match binding_pat with
      | PatIdent ident ->
          let ptr =
            build_alloca (get_llvm_ty (Option.get binding_ty)) "" builder
          in
          let expr = gen_expr builder binding_expr in
          ignore (build_store expr ptr builder);
          Hashtbl.add !scope.bindings ident ptr)
    | _ -> assert false
  in
  let tmp_scope = !scope in
  scope := { bindings = Hashtbl.create 0; parent = Some tmp_scope };
  List.iter f block.block_stmts;
  ignore
    (match block.last_expr with
    | Some expr ->
        let ret_expr = gen_expr builder expr in
        build_ret ret_expr builder
    | None -> build_ret_void builder);
  scope := tmp_scope

let gen_func (func : func) (ll_mod : llmodule) =
  let function_type = gen_function_type func.fn_sig in
  if func.is_extern then assert false
  else (
    let fn = define_function func.fn_sig.name function_type ll_mod in
    let builder = builder_at_end ctx (entry_block fn) in
    for i = 0 to Array.length (params fn) - 1 do
      let _, name = List.nth func.fn_sig.args i in
      let arg = Array.get (params fn) i in
      let ty = type_of arg in
      set_value_name name arg;
      let ptr = build_alloca ty "" builder in
      ignore (build_store arg ptr builder);
      Hashtbl.add !scope.bindings name ptr
    done;
    match func.body with Some body -> gen_block builder body | None -> ())

let gen_item (item : item) (ll_mod : llmodule) =
  match item with Fn (func, _) -> gen_func func ll_mod | _ -> assert false

let gen_module (name : string) (modd : modd) : llmodule =
  let ll_mod = create_module ctx name in
  ignore (List.map (fun item -> gen_item item ll_mod) modd.items);
  ll_mod

let emit (modd : llmodule) (out : string) =
  initialize ();
  x86AsmPrinterInit ();
  let triple = Target.default_triple () in
  set_target_triple triple modd;
  let target = Target.by_triple triple in
  let machine =
    TargetMachine.create ~triple ?cpu:(Some "generic") ?features:None
      ?level:(Some CodeGenOptLevel.Default)
      ?reloc_mode:(Some RelocMode.Static)
      ?code_model:(Some CodeModel.Default) target
  in
  dump_module modd;
  let objfile = out ^ ".o" in
  TargetMachine.emit_to_file modd CodeGenFileType.ObjectFile objfile machine;
  let command = Sys.command ("clang " ^ objfile ^ " -o " ^ out) in
  ignore (Sys.command ("rm " ^ objfile));
  if command <> 0 then Printf.fprintf stderr "cannot emit executable\n"
