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

let jmpbuf = "__ina_jmpbuf"
let define_jmpbuf () = prelude ^ sprintf "static jmp_buf %s;\n" jmpbuf
let newline () = out ^ "\n"

type cx = {
    tcx: tcx
  ; irmdl: Ir.Module.t
  ; types: string TypeMap.t
  ; defined_types: (string, unit) hashmap
  ; gen'd_fns: (instance, unit) hashmap
  ; gen'd_generics: (instance, unit) hashmap
  ; tests: instance vec
}

let rec backend_ty cx ty =
  !ty
  |> function
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
  | Tuple tys as ty' ->
      (match TypeMap.find_opt cx.types ty' with
       | Some ty -> ty
       | None ->
           let name = sprintf "__tuple_%d" (TypeMap.length cx.types) in
           prelude ^ sprintf "// %s\n" (cx.tcx#render_ty ty);
           let tys =
             mapi tys (fun i ty -> sprintf "%s _%d;" (backend_ty cx ty) i)
           in
           let body = tys#join ";" Fun.id in
           prelude
           ^ sprintf "typedef struct %s { %s } %s;\n\n" name body name;
           TypeMap.add cx.types ty' name;
           name)
  | Slice ty as ty' ->
      (match TypeMap.find_opt cx.types ty' with
       | Some ty -> ty
       | None ->
           let ty = backend_ty cx ty in
           let name = sprintf "__slice_%d" (TypeMap.length cx.types) in
           prelude ^ sprintf "// %s\n" (cx.tcx#render_ty (ref ty'));
           prelude
           ^ sprintf
               "typedef struct %s { %s* ptr; size_t length; } %s;\n\n"
               name
               ty
               name;
           TypeMap.add cx.types ty' name;
           name)
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
  let kind = cx.tcx#adt_kind ty in
  match !ty with
  | Adt _ when Option.get kind = StructT ->
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
  | Adt _ ->
      (* cx.tcx#render_ty ty |> print_endline; *)
      cx.defined_types#insert' name ();
      let variants = cx.tcx#variants ty |> Option.get in
      let i = ref 0 in
      let variants =
        variants#join "\n" (function Variant { fields; _ } ->
            let f =
              fields#join "\n" (function Field { ty; name } ->
                  let name' = backend_ty cx ty in
                  (match cx.defined_types#get name' with
                   | Some () -> ()
                   | None -> define cx name' ty);
                  sprintf "  %s _%s;" (backend_ty cx ty) name)
            in
            let name = String.cat name @@ string_of_int !i in
            if not fields#empty
            then
              prelude
              ^ sprintf "typedef struct %s {\n%s\n} %s;\n" name f name;
            let field = sprintf "    %s _%d;" name !i in
            incr i;
            if not fields#empty then field else "")
      in
      let union = sprintf "  union {\n%s\n  } data;" variants in
      prelude ^ sprintf "// %s\n" (cx.tcx#render_ty ty);
      prelude
      ^ sprintf
          "typedef struct %s {\n  uint8_t discriminant;\n%s\n} %s;\n\n"
          name
          union
          name
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
  ; gen'd_generics = new hashmap
  ; tests = new vec
  }
;;

let gen cx =
  let is_test = ref false in
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
         | Fn id | Test id ->
             let name = Mangle.mangle cx.tcx instance in
             let ty = cx.tcx#fn id instance.subst in
             let generics = cx.tcx#generics_of (instance_def_id instance) in
             (match cx.gen'd_fns#get instance with
              | None when id.mod_id <> 0 && Generics.count generics = 0 ->
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
    | "transmute" ->
        let open Errors in
        let src = args#get 0 in
        let ty = get_ty cx.tcx src in
        let fn = get_ty cx.tcx value in
        let dst = Middle.Ty.Fn.ret cx.tcx fn in
        (if cx.tcx#sizeof ty <> cx.tcx#sizeof dst
         then
           let msg =
             sprintf
               "invalid transmute between types `%s` and `%s`"
               (cx.tcx#render_ty ty)
               (cx.tcx#render_ty dst)
           in
           Diagnostic.create msg |> cx.tcx#emit);
        out ^ sprintf "*(%s*)(&%s);\n" (backend_ty cx dst) (get_value src)
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
    | Aggregate (Adt (did, vidx, s), args) ->
        let ty = cx.tcx#adt did s in
        let ty' = backend_ty cx ty in
        let data =
          if not args#empty
          then
            let args = map args get_value in
            let args = mapi args (fun i arg -> sprintf "._%d = %s" i arg) in
            let args = args#join ", " Fun.id in
            let vname = String.cat ty' @@ string_of_int vidx in
            sprintf "{ ._%d = (%s) { %s } }" vidx vname args
          else "{}"
        in
        out ^ sprintf "{ .discriminant = %d, .data = %s };\n" vidx data
    | Aggregate (Slice ty, args) ->
        let ty = cx.tcx#inner_ty ty |> Option.get in
        let length = args#len in
        let data =
          if args#empty
          then "{}"
          else
            let args = args#join ", " get_value in
            sprintf
              "{ .ptr = (%s[]){ %s }, .length = %d }"
              (backend_ty cx ty)
              args
              length
        in
        out ^ sprintf "%s;\n" data
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
        (match !ty with
         | Adt _ ->
             let (Variant variant) =
               cx.tcx#non_enum_variant ty |> Option.get
             in
             let (Field { name; _ }) = variant.fields#get index in
             out ^ sprintf "&%s->%s;\n" (get_value ptr) name
         | Tuple _ -> out ^ sprintf "&%s->_%d;\n" (get_value ptr) index
         | _ -> assert false)
    | Index (_, value, index) ->
        out ^ sprintf "%s.ptr[%s];\n" (get_value value) (get_value index)
    | Len value -> out ^ sprintf "%s.length;\n" (get_value value)
    | BitCast (value, ty)
    | IntToPtr (value, ty)
    | PtrToInt (value, ty)
    | Trunc (value, ty)
    | Zext (value, ty) ->
        out ^ sprintf "(%s)%s;\n" (backend_ty cx ty) (get_value value)
    | Trap value when !is_test ->
        out ^ sprintf "__builtin_printf(\"%s\");\n" (String.escaped value);
        out ^ sprintf "  longjmp(%s, 1);\n" jmpbuf
    | Trap value ->
        out ^ sprintf "__builtin_printf(\"%s\");\n" (String.escaped value);
        out ^ "  __builtin_abort();\n"
    | Discriminant value ->
        out ^ sprintf "(%s).discriminant;\n" (get_value value)
    | Payload (value, _) ->
        out
        ^ sprintf
            "(%s)(&%s.data);\n"
            (backend_ty cx inst.ty)
            (get_value value)
    | _ ->
        newline ();
        print_endline @@ render_inst cx.tcx inst;
        assert false
  and gen_terminator = function
    | Switch (value, branches, default) ->
        out
        ^ sprintf
            "switch (%s) { %s %s };\n"
            (get_value value)
            (branches#join " " (fun (bb, value) ->
                 sprintf "case %s: goto %s;" (get_value value) (get_value bb)))
            (default
             |> Option.map (fun bb ->
                    sprintf "default: goto %s;" (get_value bb))
             |> Option.value ~default:"default: __builtin_unreachable();")
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
  and gen_test func =
    let open Ir.Func in
    let name = Mangle.mangle cx.tcx func.instance in
    cx.tests#push func.instance;
    let header = sprintf "void %s()" name in
    out ^ header;
    out ^ " {\n";
    func.basic_blocks.locals#iter gen_inst;
    func.basic_blocks.bbs#iter gen_bb;
    out ^ "}\n\n"
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
      | Test _ ->
          is_test := true;
          gen_test f;
          is_test := false
      | _ ->
          if not (cx.gen'd_fns#has f.instance)
          then (
            assert (cx.gen'd_fns#insert f.instance () = None);
            if cx.tcx#is_generic f.ty
            then cx.gen'd_generics#insert' f.instance ();
            gen_function f));
  (match cx.tcx#main with
   | _ when cx.tcx#sess.options.command = Test ->
       prelude ^ sprintf "#include <setjmp.h>\n";
       define_jmpbuf ();
       let total = cx.tests#len in
       let g = sprintf "\\x1b[32m%s\\x1b[0m" in
       let r = sprintf "\\x1b[31m%s\\x1b[0m" in
       let ok_str = g "[     OK ]" in
       let failed_str = r "[ FAILED ]" in
       let info = g "[ INFO   ]" in
       let sep = g "[========]" in
       let run instance =
         let name = Mangle.mangle cx.tcx instance in
         let qpath = cx.tcx#qpath ~full:true (instance_def_id instance) in
         sprintf
           "if (setjmp(%s) == 0) {\n\
           \    %s();\n\
           \    __builtin_printf(\"%s %s\\n\");\n\
           \    passed++;\n\
           \  } else {\n\
           \    __builtin_printf(\"%s %s\\n\");\n\
           \    failed++;\n\
           \  }"
           jmpbuf
           name
           ok_str
           qpath
           failed_str
           qpath
       in
       let tests = cx.tests#join "\n  " run in
       let result = "\" %zu passed, %zu failed\\n\\n\", passed, failed" in
       out ^ "int main() {\n";
       out ^ "  size_t passed = 0; size_t failed = 0;\n";
       out
       ^ sprintf
           "  __builtin_printf(\"%s running %d test%s\\n\");\n"
           info
           total
           (if total = 1 then "" else "s");
       out ^ "  ";
       out ^ tests;
       newline ();
       out ^ sprintf "  __builtin_printf(\"%s\\n\");\n" sep;
       out
       ^ sprintf
           "  __builtin_printf(passed == %d ? \"%s\" : \"%s\");\n"
           total
           ok_str
           failed_str;
       out ^ sprintf "  __builtin_printf(%s);\n" result;
       out ^ "  return failed != 0;\n";
       out ^ "}\n"
   | Some id ->
       let instance = { def = Fn id; subst = Subst (new vec) } in
       gen_main instance
   | _ when cx.tcx#sess.options.output_type = Exe -> assert false
   | _ -> ());
  Metadata.Encoder.encode_hashmap
    cx.tcx#sess.enc
    cx.gen'd_generics
    encode_instance
    (fun _ _ -> ());
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
