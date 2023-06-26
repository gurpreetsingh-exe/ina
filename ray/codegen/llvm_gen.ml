open Ast
open Ir
open Llvm
open Llvm_target
open Llvm_X86
open Llvm_analysis
open Resolve.Imports
open Printf
open Session

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
  mutable curr_fn : llvalue option;
  func_map : (path, lltype) Hashtbl.t;
  mutable main : llvalue option;
  intrinsics : (path, Func.fn_type) Hashtbl.t;
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
    curr_fn = None;
    func_map = Hashtbl.create 0;
    main = None;
    intrinsics = Hashtbl.create 0;
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

let gen_function_type (fn_ty : Func.fn_type) : lltype =
  let args = List.map (fun (ty, _) -> get_llvm_ty ty) fn_ty.args in
  let ret_ty = get_llvm_ty fn_ty.ret_ty in
  (if fn_ty.is_variadic then var_arg_function_type else function_type)
    ret_ty (Array.of_list args)

let gen_main ll_mod name =
  let main = Option.get codegen_ctx.main in
  let main_ty = function_type (get_llvm_ty (Prim I32)) [||] in
  let fn = define_function "main" main_ty ll_mod in
  let builder = builder_at_end codegen_ctx.llctx (entry_block fn) in
  let ret = build_call main_ty main [||] "" builder in
  ignore (build_ret ret builder)

let gen_blocks (blocks : Func.blocks) =
  let fn = Option.get codegen_ctx.curr_fn in
  let insts = Hashtbl.create 0 in
  let bbs = Hashtbl.create 0 in
  List.iter
    (fun (bb : Inst.basic_block) ->
      Hashtbl.add bbs bb.bid
        (if bb.is_entry then entry_block fn
        else append_block codegen_ctx.llctx (sprintf "bb%d" bb.bid) fn))
    blocks.bbs;
  let get_value (value : Inst.value) : llvalue =
    match value with
    | Const (const, ty) -> (
        let ty = get_llvm_ty ty in
        match const with
        | Int value -> const_int ty value
        | Float value -> const_float ty value
        | Str value ->
            let id =
              Printf.sprintf "str%d"
                (Hashtbl.length codegen_ctx.global_strings)
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
                global_ptr;
                const_int codegen_ctx.size_type (String.length value);
              |]
        | Bool value -> const_int ty (if value then 1 else 0))
    | VReg (inst, id, _) -> Hashtbl.find insts id
    | Label bb -> value_of_block (Hashtbl.find bbs bb.bid)
    | Param (_, _, i) -> (params (Option.get codegen_ctx.curr_fn)).(i)
  in
  let get_load_ty ptr =
    match Inst.get_ty ptr with
    | Ptr ty -> ty
    | RefTy ty -> ty
    | _ -> assert false
  in
  let rec g (inst : Inst.t) builder =
    let llinst =
      match inst.kind with
      | Alloca ty -> Some (build_alloca (get_llvm_ty ty) "" builder)
      | Binary (kind, left, right) ->
          let pty = Inst.get_ty left in
          let left = get_value left in
          let right = get_value right in
          let is_float = is_float pty in
          let op =
            match kind with
            | Add -> build_add
            | Sub -> build_sub
            | Mul -> build_mul
            | Div -> (
              match pty with
              | Prim t ->
                  if is_unsigned t then build_udiv
                  else if is_signed t then build_sdiv
                  else assert false
              | _ -> assert false)
            | _ ->
                if is_float then assert false
                else
                  build_icmp
                    (match kind with
                    | Eq -> Icmp.Eq
                    | NotEq -> Icmp.Ne
                    | _ -> assert false)
          in
          Some (op left right "" builder)
      | Br (cond, true_bb, false_bb) ->
          let cond = get_value cond in
          ignore
            (build_cond_br cond
               (block_of_value (get_value true_bb))
               (block_of_value (get_value false_bb))
               builder);
          None
      | Jmp bb -> Some (build_br (block_of_value (get_value bb)) builder)
      | Phi (ty, values) ->
          Some
            (build_phi
               (List.map
                  (fun (bb, inst) ->
                    (get_value inst, block_of_value (get_value bb)))
                  values)
               "" builder)
      | Ret value -> Some (build_ret (get_value value) builder)
      | RetUnit -> Some (build_ret_void builder)
      | Store (src, dst) ->
          Some (build_store (get_value src) (get_value dst) builder)
      | Load ptr ->
          Some
            (build_load
               (get_llvm_ty (get_load_ty ptr))
               (get_value ptr) "" builder)
      | Call (ty, name, args) ->
          let fn_ty =
            match ty with
            | FnTy (args, ret_ty, is_variadic) ->
                let args = List.map (fun ty -> get_llvm_ty ty) args in
                let ret_ty = get_llvm_ty ret_ty in
                (if is_variadic then var_arg_function_type
                else function_type)
                  ret_ty (Array.of_list args)
            | _ -> assert false
          in
          let args = Array.map get_value (Array.of_list args) in
          let fn =
            Option.get
              (lookup_function name (Option.get codegen_ctx.curr_mod))
          in
          Some (build_call fn_ty fn args "" builder)
      | Intrinsic (name, args) ->
          let args = Array.map get_value (Array.of_list args) in
          Some (Intrinsics.gen_intrinsic name args builder codegen_ctx.llctx)
      | Nop -> None
      (* | _ -> *)
      (*     print_endline (Inst.render_inst inst); *)
      (*     assert false *)
    in
    match llinst with
    | Some instr -> Hashtbl.add insts inst.id instr
    | None -> ()
  and f (bb : Inst.basic_block) =
    let llbb = Hashtbl.find bbs bb.bid in
    let builder = builder_at_end codegen_ctx.llctx llbb in
    List.iter (fun inst -> g inst builder) bb.insts
  in
  List.iter f blocks.bbs

let gen_item (func : Func.t) (ll_mod : llmodule) =
  let intrinsic (fn : Func.fn_type) declare =
    let fn_ty = gen_function_type fn in
    if fn.abi = "intrinsic" then
      Hashtbl.add codegen_ctx.intrinsics { segments = [fn.name] } fn
    else if declare then (
      let llfn = declare_function fn.name fn_ty ll_mod in
      codegen_ctx.curr_fn <- Some llfn;
      Hashtbl.add codegen_ctx.func_map { segments = [fn.name] } fn_ty;
      if fn.name = "main" then codegen_ctx.main <- Some llfn;
      set_linkage Linkage.External llfn)
    else (
      let llfn = define_function fn.linkage_name fn_ty ll_mod in
      codegen_ctx.curr_fn <- Some llfn;
      if fn.name = "main" then codegen_ctx.main <- Some llfn)
  in
  match func with
  | Decl fn_ty -> intrinsic fn_ty true
  | Def { def_ty; basic_blocks } ->
      intrinsic def_ty false; gen_blocks basic_blocks

let gen_module (ctx : Context.t) (modd : Module.t) : llmodule =
  let ll_mod = create_module codegen_ctx.llctx ctx.options.input in
  codegen_ctx.curr_mod <- Some ll_mod;
  set_target_triple (Target.default_triple ()) ll_mod;
  ignore (List.map (fun item -> gen_item item ll_mod) modd.items);
  gen_main ll_mod ctx.options.input;
  match verify_module ll_mod with
  | Some reason ->
      Printf.fprintf stderr "%s\n" reason;
      ll_mod
  | None -> ll_mod

let emit (modd : llmodule) (ctx : Context.t) =
  (match ctx.options.opt_level with
  | Default -> ()
  | Agressive -> run_passes modd "default<O3>" codegen_ctx.machine);
  match ctx.options.output_type with
  | LlvmIr ->
      let ic = open_out (ctx.options.output ^ ".ll") in
      output_string ic (string_of_llmodule modd)
  | Object ->
      let objfile = ctx.options.output ^ ".o" in
      TargetMachine.emit_to_file modd CodeGenFileType.ObjectFile objfile
        codegen_ctx.machine
  | Asm ->
      TargetMachine.emit_to_file modd CodeGenFileType.AssemblyFile
        (ctx.options.output ^ ".s")
        codegen_ctx.machine
  | Exe ->
      let objfile = ctx.options.output ^ ".o" in
      TargetMachine.emit_to_file modd CodeGenFileType.ObjectFile objfile
        codegen_ctx.machine;
      let command =
        Sys.command ("clang " ^ objfile ^ " -o " ^ ctx.options.output)
      in
      if command <> 0 then Printf.fprintf stderr "cannot emit executable\n";
      ignore (Sys.command ("rm -f " ^ objfile))
