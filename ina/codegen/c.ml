open Middle.Ctx
open Middle.Ty
open Middle.Def_id
open Structures.Vec
open Structures.Hashmap
open Printf
open Ir.Inst

let prelude =
  ref "#include <sys/types.h>\n#include <stdint.h>\n#include <stdbool.h>\n\n"
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
  ; types: string TypeMap.t
  ; defined_types: (string, unit) hashmap
  ; gen'd_fns: (instance, unit) hashmap
}

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
  | Ptr (_, ty) | Ref (_, ty) -> sprintf "%s*" (backend_ty cx ty)
  | Unit -> "void"
  | Str as ty ->
      (match TypeMap.find_opt cx.types ty with
       | Some ty -> ty
       | None ->
           let name = "__ina_string" in
           prelude
           ^ sprintf
               "typedef struct %s {\n\
               \  void* ptr;\n\
               \  size_t length;\n\
                } %s;\n\n"
               name
               name;
           TypeMap.add cx.types ty name;
           name)
  | Adt _ as ty' ->
      (match TypeMap.find_opt cx.types ty' with
       | Some ty -> ty
       | None ->
           let name = Mangle.mangle_ty cx.tcx ty in
           prelude ^ sprintf "// %s\n" (cx.tcx#render_ty ty);
           prelude ^ sprintf "typedef struct %s %s;\n" name name;
           TypeMap.add cx.types ty' name;
           define cx name ty;
           name)
  | FnPtr { args; ret; is_variadic; _ } as ty ->
      (match TypeMap.find_opt cx.types ty with
       | Some ty -> ty
       | None ->
           let name = sprintf "__fn_%d" (TypeMap.length cx.types) in
           prelude
           ^ sprintf
               "typedef %s (*%s)(%s%s);\n\n"
               (backend_ty cx ret)
               name
               (args#join ", " (cx |> backend_ty))
               (match is_variadic, args#empty with
                | true, true -> "..."
                | true, false -> ", ..."
                | _ -> "");
           TypeMap.add cx.types ty name;
           name)
  | Fn _ as ty -> fn cx ty
  | _ ->
      print_endline @@ Middle.Ty.render_ty2 ty;
      assert false

and fn cx ty =
  match TypeMap.find_opt cx.types ty with
  | Some ty -> ty
  | None ->
      let ty = cx.tcx#ty_with_subst (ref ty) in
      let { args; ret; is_variadic; _ } = Fn.get cx.tcx ty in
      let name = sprintf "__fn_%d" (TypeMap.length cx.types) in
      prelude
      ^ sprintf
          "typedef %s (*%s)(%s%s);\n\n"
          (backend_ty cx ret)
          name
          (args#join ", " (cx |> backend_ty))
          (match is_variadic, args#empty with
           | true, true -> "..."
           | true, false -> ", ..."
           | _ -> "");
      TypeMap.add cx.types !ty name;
      name

and define cx name ty =
  match !ty with
  | Adt _ ->
      cx.defined_types#insert' name ();
      let (Variant variant) = cx.tcx#non_enum_variant ty |> Option.get in
      let fields =
        variant.fields#join "\n" (fun (Field { ty; name }) ->
            let name' = backend_ty cx ty in
            (match cx.defined_types#get name' with
             | Some () -> ()
             | None -> define cx name' ty);
            sprintf "  %s %s;" (backend_ty cx ty) name)
      in
      prelude ^ sprintf "// %s\n" (cx.tcx#render_ty ty);
      prelude ^ sprintf "typedef struct %s {\n%s\n} %s;\n\n" name fields name
  | Fn _ | FnPtr _ | Str -> ()
  | _ -> ()
;;

let create tcx irmdl =
  {
    tcx
  ; irmdl
  ; types = TypeMap.create 0
  ; defined_types = new hashmap
  ; gen'd_fns = new hashmap
  }
;;

let gen cx =
  let render_fn_header name ty =
    let ty = cx.tcx#ty_with_subst ty in
    let { args; ret; is_variadic; _ } = Fn.get cx.tcx ty in
    sprintf
      "%s %s(%s%s)"
      (backend_ty cx ret)
      name
      (args#join ", " (cx |> backend_ty))
      (match is_variadic, args#empty with
       | true, true -> "..."
       | true, false -> ", ..."
       | _ -> "")
  in
  let inst_name inst = sprintf "_%d" inst.id in
  let rec get_const ty = function
    | Int v -> string_of_int v
    | Float v -> string_of_float v
    | Bool v -> string_of_bool v
    | Str v ->
        sprintf
          "(__ina_string) { .ptr = \"%s\", .length = %d }"
          (String.escaped v)
          (String.length v)
    | Struct v ->
        let ty' = backend_ty cx ty in
        let (Variant variant) = cx.tcx#non_enum_variant ty |> Option.get in
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
    | Global (Fn instance) ->
        (match instance.def with
         | Intrinsic _ -> Mangle.mangle cx.tcx instance
         | Fn id ->
             let name = Mangle.mangle cx.tcx instance in
             let ty = cx.tcx#fn id instance.subst in
             (match cx.gen'd_fns#get instance with
              | None when id.mod_id <> 0 ->
                  cx.gen'd_fns#insert' instance ();
                  prelude ^ sprintf "// %s\n" (cx.tcx#render_ty ty);
                  prelude ^ sprintf "extern %s;\n" (render_fn_header name ty)
              | None ->
                  (* prelude ^ sprintf "// %s\n" (cx.tcx#render_ty ty); *)
                  prelude ^ sprintf "%s;\n" (render_fn_header name ty)
              | Some () -> ());
             name)
    | Const { kind; ty } -> get_const ty kind
    | Label bb -> sprintf "bb%d" bb.bid
  in
  let rec gen_bb bb =
    let open Ir.Inst in
    out ^ sprintf "bb%d:;\n" bb.bid;
    bb.insts#iter gen_inst;
    out ^ "  ";
    gen_terminator bb.terminator
  and gen_intrinsic value args =
    match get_value value with
    | "len" ->
        let first = args#get 0 in
        let ty = get_ty cx.tcx first in
        out
        ^
        (match !ty with
         | Ptr _ | Ref _ -> sprintf "((size_t*)%s)[1];\n" (get_value first)
         | _ -> sprintf "((size_t*)&%s)[1];\n" (get_value first))
    | "offset" ->
        let first = args#get 0 in
        let ty = get_ty cx.tcx first in
        out
        ^
        (match !ty with
         | Ptr _ ->
             sprintf
               "(%s + %s);\n"
               (get_value first)
               (get_value (args#get 1))
         | _ -> assert false)
    | "sizeof" ->
        let ty = get_ty cx.tcx value in
        let subst = cx.tcx#get_subst ty |> Option.get in
        let ty = subst#get 0 |> function Ty ty -> ty in
        out ^ sprintf "%d;\n" (cx.tcx#sizeof ty)
    | name ->
        print_endline name;
        assert false
  and gen_inst inst =
    out
    ^
    if has_value inst
    then sprintf "  %s %s = " (backend_ty cx inst.ty) (inst_name inst)
    else "  ";
    match inst.kind with
    | Alloca ty ->
        out ^ sprintf "__builtin_alloca(sizeof(%s));\n" (backend_ty cx ty)
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
    | Call (ty, value, args) ->
        (match Fn.abi cx.tcx ty with
         | Intrinsic -> gen_intrinsic value args
         | _ ->
             out
             ^ sprintf
                 "%s(%s);\n"
                 (get_value value)
                 (args#join ", " get_value))
    | Gep (ty, ptr, index) ->
        let (Variant variant) = cx.tcx#non_enum_variant ty |> Option.get in
        let (Field { name; _ }) = variant.fields#get index in
        out ^ sprintf "&%s->%s;\n" (get_value ptr) name
    | BitCast (value, ty)
    | IntToPtr (value, ty)
    | PtrToInt (value, ty)
    | Trunc (value, ty)
    | Zext (value, ty) ->
        out ^ sprintf "(%s)%s;\n" (backend_ty cx ty) (get_value value)
    | Trap value ->
        out ^ sprintf "  __builtin_printf(\"%s\");\n" (String.escaped value);
        out ^ "  __builtin_abort();\n"
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
    let name = Mangle.mangle cx.tcx func.instance in
    let { args; ret; is_variadic; _ } = Fn.get cx.tcx func.ty in
    let gen_arg ty arg =
      sprintf "%s %s" (backend_ty cx ty) (get_value arg)
    in
    let args = map2 args func.args gen_arg in
    let header =
      sprintf
        "%s%s %s(%s%s)"
        (* (if cx.tcx#is_extern func.def_id then "extern " else "") *)
        ""
        (backend_ty cx ret)
        name
        (args#join ", " (fun s -> s))
        (match is_variadic, args#empty with
         | true, true -> "..."
         | true, false -> ", ..."
         | _ -> "")
    in
    out ^ header;
    if func.decl
    then out ^ ";\n"
    else (
      out ^ " {\n";
      func.basic_blocks.locals#iter gen_inst;
      func.basic_blocks.bbs#iter gen_bb;
      out ^ "}\n\n")
  in
  let gen_main main =
    let name = Mangle.mangle cx.tcx main in
    let ty = cx.tcx#get_def (instance_def_id main) in
    match Fn.ret cx.tcx ty with
    | ret when ret = cx.tcx#types.unit ->
        out ^ sprintf "int main() {\n  %s();\n  return 0;\n}\n" name
    | _ -> out ^ sprintf "int main() {\n  return %s();\n}\n" name
  in
  cx.irmdl.items#iter (fun f ->
      match f.instance.def with
      | Intrinsic _ -> ()
      | _ ->
          if not (cx.gen'd_fns#has f.instance)
          then (
            assert (cx.gen'd_fns#insert f.instance () = None);
            gen_function f));
  (match cx.tcx#main with
   | Some id ->
       let instance = { def = Fn id; subst = Subst (new vec) } in
       gen_main instance
   | _ when cx.tcx#sess.options.output_type = Exe -> assert false
   | _ -> ());
  (match cx.tcx#sess.options.output_type with
   | ExtMod ->
       let data =
         cx.tcx#sess.enc#render
         |> Bytes.to_seq
         |> Array.of_seq
         |> Array.fold_left
              (fun acc c -> sprintf "%s %i," acc (int_of_char c))
              ""
       in
       let i = cx.tcx#sess.options.input in
       prelude
       ^ sprintf
           "const uint8_t __ina_metadata_%d[] %s = { %s };\n"
           (Hashtbl.hash i)
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
