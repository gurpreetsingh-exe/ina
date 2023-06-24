open Ast
open Ir
open Llvm
open Llvm_target
open Llvm_X86
open Llvm_analysis
open Resolve.Imports
open Printf
open Session

let render_path path = String.concat "::" path.segments

let mangle path =
  "_Z"
  ^ String.concat ""
      (List.map
         (fun seg -> sprintf "%d%s" (String.length seg) seg)
         path.segments)

let _bb_id = ref 0

let bb_id () = incr _bb_id; sprintf "bb%d" !_bb_id

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

let rec lvalue expr builder =
  match expr.expr_kind with
  | Path path -> find_val codegen_ctx.env (render_path path)
  | Deref expr ->
      build_load
        (pointer_type codegen_ctx.llctx)
        (lvalue expr builder) "" builder
  | _ -> assert false

let rec gen_block (builder : llbuilder) (block : block) name newbb =
  let bb =
    if newbb then (
      match name with
      | "entry" -> entry_block (Option.get codegen_ctx.curr_fn)
      | _ ->
          append_block codegen_ctx.llctx name
            (Option.get codegen_ctx.curr_fn))
    else insertion_block builder
  in
  let builder = builder_at_end codegen_ctx.llctx bb in
  let f stmt : llbuilder =
    match stmt with
    | Binding { binding_pat; binding_ty; binding_expr; _ } -> (
        let ty = Option.get binding_ty in
        let ll_ty = get_llvm_ty ty in
        match ty with
        | Unit ->
            let _, b = gen_expr builder binding_expr in
            b
        | _ -> (
          match binding_pat with
          | PatIdent ident ->
              let ptr = build_alloca ll_ty ident builder in
              let expr, b = gen_expr builder binding_expr in
              ignore (build_store expr ptr builder);
              Hashtbl.add codegen_ctx.env.bindings ident ptr;
              b))
    | Assign (expr, init) ->
        let ptr = lvalue expr builder in
        let init, b = gen_expr builder init in
        ignore (build_store init ptr builder);
        b
    | Stmt expr | Expr expr ->
        let _, b = gen_expr builder expr in
        b
  in
  let tmp_scope = codegen_ctx.env in
  codegen_ctx.env <- { bindings = Hashtbl.create 0; parent = Some tmp_scope };
  let builders = List.rev (List.map f block.block_stmts) in
  let builder =
    if List.length builders <> 0 then List.hd builders else builder
  in
  (* ignore (List.map f block.block_stmts); *)
  let ret =
    match block.last_expr with
    | Some expr ->
        let expr, builder = gen_expr builder expr in
        (Some expr, builder)
    | None -> (None, builder)
  in
  codegen_ctx.env <- tmp_scope;
  ret

and gen_expr (builder : llbuilder) (expr : expr) : llvalue * llbuilder =
  let ty = get_llvm_ty (Option.get expr.expr_ty) in
  match expr.expr_kind with
  | Lit lit ->
      ( (match lit with
        | LitInt value -> const_int ty value
        | LitFloat value -> const_float ty value
        | LitBool value -> const_int ty (if value then 1 else 0)
        | LitStr value ->
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
              |]),
        builder )
  | Path path ->
      let ptr = find_val codegen_ctx.env (render_path path) in
      (build_load ty ptr "" builder, builder)
  | Call (path, exprs) ->
      assert false
      (* let args = *)
      (*   Array.map *)
      (*     (fun expr -> *)
      (*       let e, _ = gen_expr builder expr in *)
      (*       e) *)
      (*     (Array.of_list exprs) *)
      (* in *)
      (* let f fn_ty fn args = (build_call fn_ty fn args "" builder, builder) in *)
      (* if Hashtbl.mem codegen_ctx.func_map path then ( *)
      (*   let fn_ty, fn = *)
      (*     ( Hashtbl.find codegen_ctx.func_map path, *)
      (*       Option.get *)
      (*         (lookup_function (mangle path) *)
      (*            (Option.get codegen_ctx.curr_mod)) ) *)
      (*   in *)
      (*   f fn_ty fn args) *)
      (* else ( *)
      (*   let env = Option.get codegen_ctx.globl_env in *)
      (*   let func = Hashtbl.find env path in *)
      (*   match func with *)
      (*   | Fn func -> *)
      (*       if func.is_extern && func.abi = "intrinsic" then *)
      (*         ( Intrinsics.gen_intrinsic func.fn_sig.name args builder *)
      (*             codegen_ctx.llctx, *)
      (*           builder ) *)
      (*       else ( *)
      (*         let fn_ty, fn = *)
      (*           gen_func func (Option.get codegen_ctx.curr_mod) *)
      (*         in *)
      (*         f fn_ty fn args) *)
      (*   | _ -> assert false) *)
  | Binary (kind, left, right) ->
      let left, _ = gen_expr builder left in
      let right, _ = gen_expr builder right in
      let pty = Option.get expr.expr_ty in
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
      (op left right "" builder, builder)
  | If { cond; then_block; _ } ->
      let cond, _ = gen_expr builder cond in
      let _, then_builder = gen_block builder then_block (bb_id ()) true in
      let then_block = insertion_block then_builder in
      let end_block =
        append_block codegen_ctx.llctx (bb_id ())
          (Option.get codegen_ctx.curr_fn)
      in
      ignore (build_br end_block then_builder);
      ( build_cond_br cond then_block end_block builder,
        builder_at_end codegen_ctx.llctx end_block )
  | Block block ->
      let expr, builder = gen_block builder block "" false in
      (Option.get expr, builder)
  | Deref expr ->
      let ptr, _ = gen_expr builder expr in
      (build_load ty ptr "" builder, builder)
  | Ref expr -> (lvalue expr builder, builder)

(* and gen_fn (fn : Func.t) (ll_mod : llmodule) = *)
(*   let function_type = gen_function_type func.fn_sig in *)
(*   if func.is_extern then ( *)
(*     let fn = declare_function func.fn_sig.name function_type ll_mod in *)
(*     codegen_ctx.curr_fn <- Some fn; *)
(*     Hashtbl.add codegen_ctx.func_map *)
(*       { segments = [func.fn_sig.name] } *)
(*       function_type; *)
(*     if func.fn_sig.name = "main" then codegen_ctx.main <- Some fn; *)
(*     set_linkage Linkage.External fn; *)
(*     (function_type, fn)) *)
(*   else ( *)
(*     Hashtbl.add codegen_ctx.func_map *)
(*       (Option.get func.func_path) *)
(*       function_type; *)
(*     let path = Option.get func.func_path in *)
(*     let mangled_name = mangle path in *)
(*     let fn = define_function mangled_name function_type ll_mod in *)
(*     codegen_ctx.curr_fn <- Some fn; *)
(*     if func.fn_sig.name = "main" then codegen_ctx.main <- Some fn; *)
(*     let builder = builder_at_end codegen_ctx.llctx (entry_block fn) in *)
(*     for i = 0 to Array.length (params fn) - 1 do *)
(*       let _, name = List.nth func.fn_sig.args i in *)
(*       let arg = Array.get (params fn) i in *)
(*       let ty = type_of arg in *)
(*       set_value_name name arg; *)
(*       let ptr = build_alloca ty "" builder in *)
(*       ignore (build_store arg ptr builder); *)
(*       Hashtbl.add codegen_ctx.env.bindings name ptr *)
(*     done; *)
(*     (match func.body with *)
(*     | Some body -> ( *)
(*         let ret, builder = gen_block builder body "entry" true in *)
(*         match ret with *)
(*         | Some ret -> ignore (build_ret ret builder) *)
(*         | None -> ()) *)
(*     | None -> ()); *)
(*     if not (verify_function fn) then *)
(*       Printf.fprintf stderr "llvm error: function `%s` is not valid\n" *)
(*         func.fn_sig.name; *)
(*     (function_type, fn)) *)

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
  let get_value (value : Inst.value) : llvalue =
    match value with
    | Const (const, ty) -> (
        let ty = get_llvm_ty ty in
        match const with
        | Int value -> const_int ty value
        | Float value -> const_float ty value)
    | VReg (inst, id, _) -> Hashtbl.find insts id
  in
  let get_load_ty ptr =
    match Inst.get_ty ptr with Ptr ty -> ty | _ -> assert false
  in
  let g (inst : Inst.t) builder =
    let llinst =
      match inst.kind with
      | Alloca ty -> Some (build_alloca (get_llvm_ty ty) "" builder)
      | Ret value -> Some (build_ret (get_value value) builder)
      | Store (dst, src) ->
          Some (build_store (get_value src) (get_value dst) builder)
      | Load ptr ->
          Some
            (build_load
               (get_llvm_ty (get_load_ty ptr))
               (get_value ptr) "" builder)
      | Nop -> None
      | _ ->
          print_endline (Inst.render_inst inst);
          assert false
    in
    match llinst with
    | Some instr -> Hashtbl.add insts inst.id instr
    | None -> ()
  in
  let f i (bb : Basicblock.t) =
    let llbb =
      if i = 0 then entry_block fn else append_block codegen_ctx.llctx "" fn
    in
    let builder = builder_at_end codegen_ctx.llctx llbb in
    List.iter (fun inst -> g inst builder) bb.insts
  in
  List.iteri f blocks.bbs

let gen_item (func : Func.t) (ll_mod : llmodule) =
  let intrinsic (fn : Func.fn_type) =
    let fn_ty = gen_function_type fn in
    if fn.abi = "intrinsic" then
      Hashtbl.add codegen_ctx.intrinsics { segments = [fn.name] } fn
    else if fn.is_extern then (
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
  | Decl fn_ty -> intrinsic fn_ty
  | Def { def_ty; basic_blocks } -> intrinsic def_ty; gen_blocks basic_blocks

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
