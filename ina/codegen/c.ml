open Middle.Ctx
open Middle.Ty
open Middle.Def_id
open Structures.Vec
open Structures.Hashmap
open Printf
open Ir.Inst

let prelude =
  ref
    "#include <alloca.h>\n\
     #include <stdint.h>\n\
     #include <stdlib.h>\n\
     #include <stdbool.h>\n\n"
;;

let metadata = "__attribute__((section(\".ina\")))"
let out = ref ""

let ( ^ ) s rest =
  s := !s ^ rest;
  ()
;;

let newline () = out ^ "\n"

type cx = {
    tcx: tcx
  ; irmdl: Ir.Module.t
  ; types: (ty, string) hashmap
  ; gen'd_fns: (def_id, unit) hashmap
}

let mangle cx did =
  let qpath = cx.tcx#def_id_to_qpath#unsafe_get did in
  if cx.tcx#is_extern did
  then Option.get qpath#last
  else
    String.concat
      ""
      [
        "_ZN"
      ; qpath#join "" (fun s -> sprintf "%d%s" (String.length s) s)
      ; "Ev"
      ]
;;

let rec backend_ty cx ty =
  !ty |> function
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
  | Bool -> "bool"
  | Ptr ty | Ref ty -> sprintf "%s*" (backend_ty cx ty)
  | Unit -> "void"
  | Str as ty ->
      (match cx.types#get ty with
       | Some ty -> ty
       | None ->
           prelude
           ^ "typedef struct str {\n\
             \  void* ptr;\n\
             \  size_t length;\n\
              } str;\n\n";
           assert (cx.types#insert ty "str" = None);
           "str")
  | Adt def_id as ty' ->
      (match cx.types#get ty' with
       | Some ty -> ty
       | None ->
           let (Variant variant) = cx.tcx#non_enum_variant ty in
           let fields =
             variant.fields#join "\n" (fun (Field { ty; name }) ->
                 sprintf "  %s %s;" (backend_ty cx ty) name)
           in
           let name = mangle cx def_id in
           assert (cx.types#insert ty' name = None);
           prelude
           ^ sprintf "typedef struct %s {\n%s\n} %s;\n\n" name fields name;
           name)
  | FnPtr { args; ret; is_variadic; _ } as ty ->
      (match cx.types#get ty with
       | Some ty -> ty
       | None ->
           let name = sprintf "__fn_%d" cx.types#len in
           prelude
           ^ sprintf
               "typedef %s (*%s)(%s%s);\n\n"
               (backend_ty cx ret)
               name
               (args#join ", " (cx |> backend_ty))
               (if is_variadic then ", ..." else "");
           assert (cx.types#insert ty name = None);
           name)
  | _ ->
      print_endline @@ Middle.Ty.render_ty2 ty;
      assert false
;;

let create tcx irmdl =
  { tcx; irmdl; types = new hashmap; gen'd_fns = new hashmap }
;;

let gen cx =
  let inst_name inst = sprintf "_%d" inst.id in
  let rec get_const ty = function
    | Int v -> string_of_int v
    | Float v -> string_of_float v
    | Bool v -> string_of_bool v
    | Str v ->
        sprintf
          "(str) { .ptr = \"%s\", .length = %d }"
          (String.escaped v)
          (String.length v)
    | Struct v ->
        let ty' = backend_ty cx ty in
        let (Variant variant) = cx.tcx#non_enum_variant ty in
        sprintf
          "(%s) { %s }"
          ty'
          ((mapi variant.fields (fun i (Field { name; _ }) ->
                sprintf ".%s = %s" name (get_value (v#get i))))
             #join
             ", "
             (fun s -> s))
    | _ -> assert false
  and get_value = function
    | Param (_, name, id) when name = "_" -> sprintf "_p%d" id
    | Param (_, name, _) -> name
    | VReg i -> inst_name i
    | Global id ->
        let name = mangle cx id in
        (match cx.gen'd_fns#get id with
         | None when id.unit_id <> 0 ->
             cx.gen'd_fns#insert' id ();
             let ty = cx.tcx#def_id_to_ty#unsafe_get id in
             let args, ret =
               match !ty with
               | FnPtr { args; ret; _ } -> args, ret
               | _ -> assert false
             in
             let header =
               sprintf
                 "extern %s %s(%s);\n"
                 (backend_ty cx ret)
                 name
                 (args#join ", " (cx |> backend_ty))
             in
             prelude ^ header
         | None ->
             let ty = cx.tcx#def_id_to_ty#unsafe_get id in
             let args, ret =
               match !ty with
               | FnPtr { args; ret; _ } -> args, ret
               | _ -> assert false
             in
             let header =
               sprintf
                 "%s %s(%s);\n"
                 (backend_ty cx ret)
                 name
                 (args#join ", " (cx |> backend_ty))
             in
             prelude ^ header
         | Some () -> ());
        name
    | Const { kind; ty } -> get_const ty kind
    | Label bb -> sprintf "bb%d" bb.bid
  in
  let rec gen_bb bb =
    let open Ir.Inst in
    out ^ sprintf "bb%d:;\n" bb.bid;
    bb.insts#iter gen_inst;
    out ^ "  ";
    gen_terminator bb.terminator
  and gen_inst inst =
    out
    ^
    if has_value inst
    then sprintf "  %s %s = " (backend_ty cx inst.ty) (inst_name inst)
    else "  ";
    match inst.kind with
    | Alloca ty -> out ^ sprintf "alloca(sizeof(%s));\n" (backend_ty cx ty)
    | Binary (kind, left, right) ->
        let op =
          match kind with
          | Add -> "+"
          | Sub -> "-"
          | Mul -> "*"
          | Div -> "/"
          | Eq -> "=="
          | NotEq -> "!="
          | And -> "&&"
          | BitAnd -> "&"
          | Or -> "||"
          | BitOr -> "|"
          | Gt -> ">"
          | GtEq -> ">="
          | Lt -> "<"
          | LtEq -> "<="
        in
        out ^ sprintf "%s %s %s;\n" (get_value left) op (get_value right)
    | Store (src, dst) ->
        out ^ sprintf "*%s = %s;\n" (get_value dst) (get_value src)
    | Copy ptr | Move ptr -> out ^ sprintf "*%s;\n" (get_value ptr)
    | Call (_, value, args) ->
        out
        ^ sprintf "%s(%s);\n" (get_value value) (args#join ", " get_value)
    | Gep (ty, ptr, index) ->
        let (Variant variant) = cx.tcx#non_enum_variant ty in
        let (Field { name; _ }) = variant.fields#get index in
        out ^ sprintf "&%s->%s;\n" (get_value ptr) name
    | BitCast (value, ty) ->
        if inst.ty = ty
        then out ^ sprintf "%s;\n" (get_value value)
        else out ^ sprintf "(%s)%s;\n" (backend_ty cx ty) (get_value value)
    | _ ->
        print_endline !out;
        newline ();
        print_endline @@ render_inst cx.tcx inst;
        assert false
  and gen_terminator = function
    | Ret value -> out ^ sprintf "return %s;\n" (get_value value)
    | RetUnit -> out ^ "return;\n"
    | Br (cond, then_block, else_block) ->
        out
        ^ sprintf
            "if (%s) { goto %s; } else { goto %s; }\n"
            (get_value cond)
            (get_value then_block)
            (get_value else_block)
    | Jmp bb -> out ^ sprintf "goto %s;\n" (get_value bb)
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
    let args = map2 args func.args gen_arg in
    let header =
      sprintf
        "%s%s %s(%s)"
        (if cx.tcx#is_extern func.def_id then "extern " else "")
        (backend_ty cx ret)
        name
        (args#join ", " (fun s -> s))
    in
    out ^ header;
    if cx.tcx#is_extern func.def_id
    then out ^ ";\n"
    else (
      out ^ " {\n";
      func.basic_blocks.locals#iter gen_inst;
      func.basic_blocks.bbs#iter gen_bb;
      out ^ "}\n\n")
  in
  let gen_main main_id =
    let name = mangle cx main_id in
    out ^ sprintf "int main() {\n  return %s();\n}\n" name
  in
  cx.irmdl.items#iter (fun f ->
      let did =
        if f.decl
        then cx.tcx#decl_extern (mangle cx f.def_id) f.def_id
        else f.def_id
      in
      if not (cx.gen'd_fns#has did)
      then (
        assert (cx.gen'd_fns#insert did () = None);
        gen_function f));
  (match cx.tcx#main with
   | Some id -> gen_main id
   | _ when cx.tcx#sess.options.output_type = Exe -> assert false
   | _ -> ());
  (match cx.tcx#sess.options.output_type with
   | Unit ->
       let data =
         cx.tcx#sess.enc#render
         |> Bytes.to_seq
         |> Array.of_seq
         |> Array.fold_left
              (fun acc c -> sprintf "%s %i," acc (int_of_char c))
              ""
       in
       prelude
       ^ sprintf
           "const uint8_t __ina_metadata[] %s = { %s };\n"
           metadata
           data
   | _ -> ());
  out := String.cat !prelude !out
;;

let emit _cx output =
  let ic = open_out output in
  output_string ic !out;
  close_out ic
;;
