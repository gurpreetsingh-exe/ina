open Ast
open Llvm
open Llvm_target
open Llvm_X86
open Llvm_analysis

type env = {
  bindings : (string, llvalue) Hashtbl.t;
  parent : env option;
}

type codegen_ctx = {
  llctx : llcontext;
  target : Target.t;
  machine : TargetMachine.t;
  data_layout : DataLayout.t;
  mutable env : env;
  global_strings : (string, llvalue) Hashtbl.t;
  size_type : lltype;
  mutable curr_mod : llmodule option;
  func_map : (ident, lltype) Hashtbl.t;
}

(* we define our own *)
external x86AsmPrinterInit : unit -> unit = "LLVMInitializeX86AsmPrinter"

external run_passes : llmodule -> string -> TargetMachine.t -> unit
  = "run_passes"

let _ =
  initialize ();
  x86AsmPrinterInit ();
  enable_pretty_stacktrace ()

let is_float = function Prim F64 | Prim F32 -> true | _ -> false

let codegen_ctx =
  let ctx = global_context () in
  let target = Target.by_triple (Target.default_triple ()) in
  let machine =
    TargetMachine.create
      ~triple:(Target.default_triple ())
      ?cpu:(Some "generic") ?features:None
      ?level:(Some CodeGenOptLevel.Default) ?reloc_mode:(Some RelocMode.PIC)
      ?code_model:(Some CodeModel.Default) target
  in
  let data_layout = TargetMachine.data_layout machine in
  {
    llctx = ctx;
    target;
    machine;
    data_layout = TargetMachine.data_layout machine;
    env = { bindings = Hashtbl.create 0; parent = None };
    global_strings = Hashtbl.create 0;
    size_type = DataLayout.intptr_type ctx data_layout;
    curr_mod = None;
    func_map = Hashtbl.create 0;
  }

let i32_c value = const_int (i32_type codegen_ctx.llctx) value

let rec find_val scope ident =
  if Hashtbl.mem scope.bindings ident then Hashtbl.find scope.bindings ident
  else (
    match scope.parent with
    | Some s -> find_val s ident
    | None -> assert false)

let get_llvm_ty (ty : ty) : lltype =
  let { llctx = ctx; size_type; _ } = codegen_ctx in
  match ty with
  | Ptr _ | RefTy _ -> pointer_type ctx
  | Prim ty -> (
    match ty with
    | Isize | Usize -> size_type
    | I64 | U64 -> i64_type ctx
    | I32 | U32 -> i32_type ctx
    | I16 | U16 -> i16_type ctx
    | I8 | U8 -> i8_type ctx
    | F32 -> float_type ctx
    | F64 -> double_type ctx
    | Bool -> i1_type ctx
    | Str -> struct_type ctx [|pointer_type ctx; size_type|])
  | Unit -> void_type ctx
  | FnTy _ -> assert false

let gen_function_type (fn_sig : fn_sig) : lltype =
  let args = List.map (fun (ty, _) -> get_llvm_ty ty) fn_sig.args in
  let ret_ty = get_llvm_ty (Option.value fn_sig.ret_ty ~default:Unit) in
  (if fn_sig.is_variadic then var_arg_function_type else function_type)
    ret_ty (Array.of_list args)

let rec gen_expr (builder : llbuilder) (expr : expr) : llvalue =
  let ty = get_llvm_ty (Option.get expr.expr_ty) in
  match expr.expr_kind with
  | Lit lit -> (
    match lit with
    | LitInt value -> const_int ty value
    | LitFloat value -> const_float ty value
    | LitBool value -> const_int ty (if value then 1 else 0)
    | LitStr value ->
        let id =
          Printf.sprintf "str%d" (Hashtbl.length codegen_ctx.global_strings)
        in
        let global_ptr =
          define_global id
            (const_string codegen_ctx.llctx value)
            (Option.get codegen_ctx.curr_mod)
        in
        set_linkage Linkage.Private global_ptr;
        set_unnamed_addr true global_ptr;
        set_global_constant true global_ptr;
        set_alignment 1 global_ptr;
        Hashtbl.add codegen_ctx.global_strings id global_ptr;
        const_struct codegen_ctx.llctx
          [|
            global_ptr; const_int codegen_ctx.size_type (String.length value);
          |])
  | Ident ident ->
      let ptr = find_val codegen_ctx.env ident in
      build_load ty ptr "" builder
  | Call (ident, exprs) ->
      let args =
        Array.map (fun expr -> gen_expr builder expr) (Array.of_list exprs)
      in
      let function_type = Hashtbl.find codegen_ctx.func_map ident in
      let fn =
        Option.get (lookup_function ident (Option.get codegen_ctx.curr_mod))
      in
      build_call function_type fn args "" builder
  | Binary (kind, left, right) ->
      let left = gen_expr builder left in
      let right = gen_expr builder right in
      let op =
        match kind with
        | Add -> build_add
        | Sub -> build_sub
        | Mul -> build_mul
        | Div -> build_fdiv
        | _ -> assert false
      in
      op left right "" builder
  | Deref expr ->
      let ptr = gen_expr builder expr in
      build_load ty ptr "" builder
  | Ref expr -> gen_expr builder expr

let gen_block (builder : llbuilder) (block : block) =
  let f stmt =
    match stmt with
    | Binding { binding_pat; binding_ty; binding_expr; _ } -> (
        let ty = Option.get binding_ty in
        let ll_ty = get_llvm_ty ty in
        match ty with
        | Unit -> ignore (gen_expr builder binding_expr)
        | _ -> (
          match binding_pat with
          | PatIdent ident ->
              let ptr = build_alloca ll_ty ident builder in
              let expr = gen_expr builder binding_expr in
              ignore (build_store expr ptr builder);
              Hashtbl.add codegen_ctx.env.bindings ident ptr))
    | Assign (expr, init) ->
        let rec lvalue expr =
          match expr.expr_kind with
          | Ident ident -> find_val codegen_ctx.env ident
          | Deref expr ->
              build_load
                (pointer_type codegen_ctx.llctx)
                (lvalue expr) "" builder
          | _ -> assert false
        in
        let ptr = lvalue expr in
        let init = gen_expr builder init in
        ignore (build_store init ptr builder)
    | Stmt expr -> ignore (gen_expr builder expr)
    | _ -> assert false
  in
  let tmp_scope = codegen_ctx.env in
  codegen_ctx.env <- { bindings = Hashtbl.create 0; parent = Some tmp_scope };
  List.iter f block.block_stmts;
  ignore
    (match block.last_expr with
    | Some expr ->
        let ret_expr = gen_expr builder expr in
        build_ret ret_expr builder
    | None -> build_ret_void builder);
  codegen_ctx.env <- tmp_scope

let gen_func (func : func) (ll_mod : llmodule) =
  let function_type = gen_function_type func.fn_sig in
  Hashtbl.add codegen_ctx.func_map func.fn_sig.name function_type;
  if func.is_extern then (
    let fn = declare_function func.fn_sig.name function_type ll_mod in
    set_linkage Linkage.External fn)
  else (
    let fn = define_function func.fn_sig.name function_type ll_mod in
    let builder = builder_at_end codegen_ctx.llctx (entry_block fn) in
    for i = 0 to Array.length (params fn) - 1 do
      let _, name = List.nth func.fn_sig.args i in
      let arg = Array.get (params fn) i in
      let ty = type_of arg in
      set_value_name name arg;
      let ptr = build_alloca ty "" builder in
      ignore (build_store arg ptr builder);
      Hashtbl.add codegen_ctx.env.bindings name ptr
    done;
    (match func.body with Some body -> gen_block builder body | None -> ());
    if not (verify_function fn) then
      Printf.fprintf stderr "llvm error: function `%s` is not valid\n"
        func.fn_sig.name)

let gen_item (item : item) (ll_mod : llmodule) =
  match item with Fn (func, _) -> gen_func func ll_mod | _ -> assert false

let gen_module (name : string) (modd : modd) : llmodule =
  let ll_mod = create_module codegen_ctx.llctx name in
  codegen_ctx.curr_mod <- Some ll_mod;
  set_target_triple (Target.default_triple ()) ll_mod;
  ignore (List.map (fun item -> gen_item item ll_mod) modd.items);
  match verify_module ll_mod with
  | Some reason ->
      Printf.fprintf stderr "%s\n" reason;
      ll_mod
  | None -> ll_mod

let emit (modd : llmodule) (out : string) =
  (* run_passes modd "default<O3>" codegen_ctx.machine; *)
  let ic = open_out (out ^ ".ll") in
  output_string ic (string_of_llmodule modd);
  let objfile = out ^ ".o" in
  TargetMachine.emit_to_file modd CodeGenFileType.ObjectFile objfile
    codegen_ctx.machine;
  TargetMachine.emit_to_file modd CodeGenFileType.AssemblyFile (out ^ ".s")
    codegen_ctx.machine;
  let command = Sys.command ("clang " ^ objfile ^ " -o " ^ out) in
  if command <> 0 then Printf.fprintf stderr "cannot emit executable\n"
