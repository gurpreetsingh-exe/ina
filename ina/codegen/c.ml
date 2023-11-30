open Middle.Ctx
open Structures
open Printf
open Ir.Inst

let prelude =
  "#include <alloca.h>\n#include <stdint.h>\n#include <stdlib.h>\n\n"
;;

let out = ref ""

let ( ^ ) s rest =
  s := !s ^ rest;
  ()
;;

let newline () = out ^ "\n"

type cx = {
    tcx: tcx
  ; irmdl: Ir.Module.t
}

let mangle cx def_id =
  let qpath = cx.tcx#def_id_to_qpath#unsafe_get def_id in
  String.concat
    ""
    [
      "_ZN"; qpath#join "" (fun s -> sprintf "%d%s" (String.length s) s); "Ev"
    ]
;;

let rec backend_ty cx = function
  | Middle.Ty.Int intty ->
      (match intty with
       | I8 -> "int8_t"
       | I16 -> "int16_t"
       | I32 -> "int32_t"
       | I64 -> "int64_t"
       | Isize -> "ssize_t"
       | U8 -> "uint8_t"
       | U16 -> "uint16_t"
       | U32 -> "uint32_t"
       | U64 -> "uint64_t"
       | Usize -> "size_t")
  | Float floatty -> (match floatty with F32 -> "float" | F64 -> "double")
  | Ptr ty -> sprintf "%s*" (backend_ty cx ty)
  | _ -> assert false
;;

let create tcx irmdl = { tcx; irmdl }

let gen cx =
  out ^ prelude;
  let inst_name inst = sprintf "_%d" inst.id in
  let get_const kind = render_const cx.tcx kind in
  let get_value = function
    | Param (_, name, _) -> name
    | VReg i -> inst_name i
    | Global id -> mangle cx id
    | Const { kind; _ } -> get_const kind
    | _ -> assert false
  in
  let rec gen_bb bb =
    let open Ir.Inst in
    out ^ sprintf "bb%d:;\n" bb.bid;
    bb.insts#iter gen_inst
  and gen_inst inst =
    out
    ^
    if has_value inst.kind
    then sprintf "  %s %s = " (backend_ty cx !(inst.ty)) (inst_name inst)
    else "  ";
    match inst.kind with
    | Alloca ty -> out ^ sprintf "alloca(%d);\n" (cx.tcx#sizeof ty)
    | Store (src, dst) ->
        out ^ sprintf "*%s = %s;\n" (get_value dst) (get_value src)
    | Load ptr -> out ^ sprintf "*%s;\n" (get_value ptr)
    | Ret value -> out ^ sprintf "return %s;\n" (get_value value)
    | Call (_, value, args) ->
        out
        ^ sprintf "%s(%s);\n" (get_value value) (args#join ", " get_value)
    | _ ->
        newline ();
        print_endline @@ render_inst cx.tcx inst;
        assert false
  and gen_function func =
    let open Ir.Func in
    let name = mangle cx func.def_id in
    let args, ret =
      match !(func.ty) with
      | FnPtr { args; ret; _ } -> args, ret
      | _ -> assert false
    in
    let gen_arg ty arg =
      sprintf "%s %s" (backend_ty cx ty) (get_value arg)
    in
    let args = Vec.map2 args func.args gen_arg in
    let header =
      sprintf
        "%s %s(%s)"
        (backend_ty cx ret)
        name
        (args#join ", " (fun s -> s))
    in
    out ^ header;
    out ^ " {\n";
    func.basic_blocks.bbs#iter gen_bb;
    out ^ "}\n\n"
  in
  let gen_main main_id =
    let name = mangle cx main_id in
    out ^ sprintf "int main() {\n  return %s();\n}\n" name
  in
  cx.irmdl.items#iter gen_function;
  match cx.tcx#main with
  | Some id -> gen_main id
  | _ -> out ^ "int main() {}\n"
;;

let emit _cx output =
  let ic = open_out output in
  output_string ic !out;
  close_out ic
;;
