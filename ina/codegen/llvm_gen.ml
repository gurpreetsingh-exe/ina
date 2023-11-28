open Ty
open Ir
open Llvm
open Llvm_target
open Llvm_X86
open Llvm_analysis
open Printf
open Utils

type env = {
    bindings: (string, llvalue) Hashtbl.t
  ; parent: env option
}

type codegen_ctx = {
    tcx: tcx
  ; mutable env: env
  ; global_strings: (string, llvalue) Hashtbl.t
  ; mutable curr_fn: llvalue option
  ; mutable main: llvalue option
  ; builtins: (string, lltype * string list) Hashtbl.t
}

(* we define our own *)
external x86AsmPrinterInit : unit -> unit = "LLVMInitializeX86AsmPrinter"

(* external run_passes : llmodule -> string -> TargetMachine.t -> unit *)
(*   = "run_passes" *)

let _ =
  initialize ();
  x86AsmPrinterInit ();
  enable_pretty_stacktrace ()
;;

let is_float = function Float _ -> true | _ -> false

let codegen_ctx tcx =
  let ty = tcx.lltys in
  let builtins =
    let seq : (string * (lltype * string list)) array =
      [|
         "write", (function_type ty.i32 [|ty.i32; ty.ptr; ty.size_type|], [])
       ; "abort", (function_type ty.void [||], ["noreturn"])
      |]
    in
    Hashtbl.of_seq (Array.to_seq seq)
  in
  {
    tcx
  ; env = { bindings = Hashtbl.create 0; parent = None }
  ; global_strings = Hashtbl.create 0
  ; curr_fn = None
  ; builtins
  ; main = None
  }
;;

let find_func cx name =
  let tcx = cx.tcx in
  let llmod = tcx.out_mod.inner in
  if Hashtbl.mem cx.builtins name
  then
    let ty, attrs = Hashtbl.find cx.builtins name in
    let func =
      match lookup_function name llmod with
      | Some func -> func
      | None ->
          let func = declare_function name ty llmod in
          List.iter
            (fun attr ->
              let attr = create_enum_attr tcx.out_mod.llcx attr 0L in
              add_function_attr func attr AttrIndex.Function)
            attrs;
          func
    in
    func, ty
  else assert false
;;

let rec find_val scope ident =
  if Hashtbl.mem scope.bindings ident
  then Hashtbl.find scope.bindings ident
  else
    match scope.parent with
    | Some s -> find_val s ident
    | None -> assert false
;;

let gen_function_type tcx (fn_ty : Func.fn_type) : lltype =
  let args = List.map (fun (ty, _) -> get_backend_type tcx ty) fn_ty.args in
  let ret_ty = get_backend_type tcx fn_ty.ret_ty in
  (if fn_ty.is_variadic then var_arg_function_type else function_type)
    ret_ty
    (Array.of_list args)
;;

let gen_main cx =
  let tcx = cx.tcx in
  match tcx.sess.options.output_type with
  | Exe ->
      let main = Option.get cx.main in
      let main_ty = function_type tcx.lltys.i32 [||] in
      let fn = define_function "main" main_ty tcx.out_mod.inner in
      let builder = builder_at_end tcx.out_mod.llcx (entry_block fn) in
      let ret = build_call main_ty main [||] "" builder in
      ignore (build_ret ret builder)
  | _ -> ()
;;

let gen_blocks (cx : codegen_ctx) (blocks : Func.blocks) =
  let tcx = cx.tcx in
  let fn = Option.get cx.curr_fn in
  let Ty.Module.{ inner = llmod; llcx } = tcx.out_mod in
  let insts = Hashtbl.create 0 in
  let bbs = Hashtbl.create 0 in
  List.iter
    (fun (bb : Inst.basic_block) ->
      Hashtbl.add
        bbs
        bb.bid
        (if bb.is_entry
         then entry_block fn
         else append_block tcx.out_mod.llcx (sprintf "bb%d" bb.bid) fn))
    blocks.bbs;
  let rec get_value (value : Inst.value) : llvalue =
    match value with
    | Const (const, ty) ->
        let ty = get_backend_type tcx ty in
        (match const with
         | Struct values ->
             const_struct llcx (Array.of_list (List.map get_value values))
         | Int value -> const_int ty value
         | Float value -> const_float ty value
         | Str value ->
             let id =
               Printf.sprintf "str%d" (Hashtbl.length cx.global_strings)
             in
             let global_ptr =
               define_global id (const_string llcx (value ^ "\000")) llmod
             in
             set_linkage Linkage.Private global_ptr;
             set_unnamed_addr true global_ptr;
             set_global_constant true global_ptr;
             set_alignment 1 global_ptr;
             Hashtbl.add cx.global_strings id global_ptr;
             const_struct
               llcx
               [|
                  global_ptr
                ; const_int tcx.lltys.size_type (String.length value)
               |]
         | Bool value -> const_int ty (if value then 1 else 0))
    | VReg (_, id, _) -> Hashtbl.find insts id
    | Label bb -> value_of_block (Hashtbl.find bbs bb.bid)
    | Param (_, _, i) -> (params (Option.get cx.curr_fn)).(i)
    | Global name ->
        (match lookup_function name llmod with
         | Some f -> f
         | None ->
             let ty =
               lookup_def tcx @@ lookup_sym2 tcx name |> function
               | Ty ty -> ty
             in
             let fn_ty =
               match ty with
               | FnTy (args, ret_ty, is_variadic) ->
                   let args =
                     List.map (fun ty -> get_backend_type tcx ty) args
                   in
                   let ret_ty = get_backend_type tcx ret_ty in
                   (if is_variadic
                    then var_arg_function_type
                    else function_type)
                     ret_ty
                     (Array.of_list args)
               | _ -> assert false
             in
             declare_function name fn_ty llmod)
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
      | Alloca ty -> Some (build_alloca (get_backend_type tcx ty) "" builder)
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
            | Div ->
                (match pty with
                 | Int t ->
                     if is_unsigned t
                     then build_udiv
                     else if is_signed t
                     then build_sdiv
                     else assert false
                 | _ -> assert false)
            | And -> build_and
            | Or -> build_or
            | _ ->
                if is_float
                then assert false
                else
                  let signed =
                    pty |> function
                    | Int ty -> is_signed ty
                    | Bool -> true
                    | _ -> assert false
                  in
                  build_icmp
                    (match kind with
                     | Eq -> Icmp.Eq
                     | NotEq -> Icmp.Ne
                     | Lt when signed -> Icmp.Slt
                     | Gt when signed -> Icmp.Sgt
                     | Lt when not signed -> Icmp.Ult
                     | Gt when not signed -> Icmp.Ugt
                     | _ -> assert false)
          in
          Some (op left right "" builder)
      | Br (cond, true_bb, false_bb) ->
          let cond = get_value cond in
          ignore
            (build_cond_br
               cond
               (block_of_value (get_value true_bb))
               (block_of_value (get_value false_bb))
               builder);
          None
      | Jmp bb -> Some (build_br (block_of_value (get_value bb)) builder)
      | Phi (_, values) ->
          Some
            (build_phi
               (List.map
                  (fun (bb, inst) ->
                    get_value inst, block_of_value (get_value bb))
                  values)
               ""
               builder)
      | Ret value -> Some (build_ret (get_value value) builder)
      | RetUnit -> Some (build_ret_void builder)
      | Store (src, dst) ->
          Some (build_store (get_value src) (get_value dst) builder)
      | Load ptr ->
          Some
            (build_load
               (get_backend_type tcx (get_load_ty ptr))
               (get_value ptr)
               ""
               builder)
      | Gep (ty, value, index) ->
          let ty = get_backend_type tcx ty in
          Some
            (build_gep
               ty
               (get_value value)
               [|const_int tcx.lltys.i32 0; const_int tcx.lltys.i32 index|]
               ""
               builder)
      | Call (ty, fn, args) ->
          let fn_ty =
            match ty with
            | FnTy (args, ret_ty, is_variadic) ->
                let args =
                  List.map (fun ty -> get_backend_type tcx ty) args
                in
                let ret_ty = get_backend_type tcx ret_ty in
                (if is_variadic then var_arg_function_type else function_type)
                  ret_ty
                  (Array.of_list args)
            | _ -> assert false
          in
          let args = Array.map get_value (Array.of_list args) in
          let fn = get_value fn in
          Some (build_call fn_ty fn args "" builder)
      | Intrinsic (name, args) ->
          let args = Array.map get_value (Array.of_list args) in
          Some (Intrinsics.gen_intrinsic name args builder tcx)
      | Nop -> None
      | Trap msg ->
          let msg = get_value msg in
          let ptr = Intrinsics.gen_intrinsic "as_ptr" [|msg|] builder tcx in
          let len = Intrinsics.gen_intrinsic "len" [|msg|] builder tcx in
          let func, write_ty = find_func cx "write" in
          ignore
            (build_call
               write_ty
               func
               [|const_int tcx.lltys.i32 2; ptr; len|]
               ""
               builder);
          let func, abort_ty = find_func cx "abort" in
          ignore (build_call abort_ty func [||] "" builder);
          ignore (build_unreachable builder);
          None
      | PtrToInt (value, ty) ->
          let value = get_value value in
          Some (build_ptrtoint value (get_backend_type tcx ty) "" builder)
      | IntToPtr (value, ty) ->
          let value = get_value value in
          Some (build_inttoptr value (get_backend_type tcx ty) "" builder)
    in
    match llinst with
    | Some instr -> Hashtbl.add insts inst.id instr
    | None -> ()
  and f (bb : Inst.basic_block) =
    let llbb = Hashtbl.find bbs bb.bid in
    let builder = builder_at_end llcx llbb in
    List.iter (fun inst -> g inst builder) bb.insts
  in
  List.iter f blocks.bbs
;;

let gen_item cx (func : Func.t) =
  let tcx = cx.tcx in
  let llmod = tcx.out_mod.inner in
  let intrinsic (fn : Func.fn_type) declare =
    let fn_ty = gen_function_type tcx fn in
    if declare
    then (
      let llfn = declare_function fn.name fn_ty llmod in
      cx.curr_fn <- Some llfn;
      if fn.name = "main" then cx.main <- Some llfn;
      set_linkage Linkage.External llfn)
    else
      let llfn =
        match lookup_function fn.linkage_name llmod with
        | Some f ->
            let fn =
              define_function
                (fn.linkage_name ^ string_of_int @@ Hashtbl.hash f)
                fn_ty
                llmod
            in
            replace_all_uses_with f fn;
            delete_function f;
            fn
        | None -> define_function fn.linkage_name fn_ty llmod
      in
      set_value_name fn.linkage_name llfn;
      cx.curr_fn <- Some llfn;
      if fn.name = "main" then cx.main <- Some llfn
  in
  match func with
  | Decl fn_ty -> intrinsic fn_ty true
  | Def { def_ty; basic_blocks } ->
      intrinsic def_ty false;
      gen_blocks cx basic_blocks
;;

let gen_module (cx : codegen_ctx) (modd : Module.t) =
  let tcx = cx.tcx in
  let llmod = tcx.out_mod.inner in
  ignore (List.map (fun item -> gen_item cx item) modd.items);
  gen_main cx;
  (match verify_module llmod with
   | Some reason -> Printf.fprintf stderr "%s\n" reason
   | None -> ());
  ()
;;

(* match tcx.sess.options.opt_level with *)
(* | Default -> () *)
(* | Agressive -> run_passes llmod "default<O3>" tcx.sess.machine *)

let emit (cx : codegen_ctx) =
  let tcx = cx.tcx in
  let llmod = tcx.out_mod.inner in
  let llcx = tcx.out_mod.llcx in
  let output = tcx.sess.options.output in
  let machine = tcx.sess.machine in
  let content = const_string llcx (tcx_metadata tcx) in
  let metadata = define_global "__ina_metadata" content llmod in
  set_section ".ina" metadata;
  set_linkage Private metadata;
  match tcx.sess.options.output_type with
  | LlvmIr ->
      let ic = open_out (output ^ ".ll") in
      output_string ic (string_of_llmodule llmod)
  | Unit ->
      let objfile = Path.with_ext (Path.add_suffix output "lib") ".o" in
      TargetMachine.emit_to_file
        llmod
        CodeGenFileType.ObjectFile
        objfile
        machine
  | Object ->
      let objfile = output ^ ".o" in
      TargetMachine.emit_to_file
        llmod
        CodeGenFileType.ObjectFile
        objfile
        machine
  | Asm ->
      TargetMachine.emit_to_file
        llmod
        CodeGenFileType.AssemblyFile
        (output ^ ".s")
        machine
  | Exe ->
      let objfile = output ^ ".o" in
      let link_args =
        " " ^ String.concat " " (List.of_seq @@ Hashtbl.to_seq_keys tcx.units)
      in
      TargetMachine.emit_to_file
        llmod
        CodeGenFileType.ObjectFile
        objfile
        machine;
      let command =
        Sys.command @@ sprintf "clang %s%s -o %s" objfile link_args output
      in
      if command <> 0 then Printf.fprintf stderr "cannot emit executable\n";
      ignore (Sys.command ("rm -f " ^ objfile))
;;
