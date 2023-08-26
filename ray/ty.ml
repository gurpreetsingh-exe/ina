open Printf
open Llvm
open Llvm_target
open Session
open Metadata

type int_ty =
  | I8
  | I16
  | I32
  | I64
  | Isize
  | U8
  | U16
  | U32
  | U64
  | Usize
[@@deriving enum]

let display_int_ty = function
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | Isize -> "isize"
  | U8 -> "u8"
  | U16 -> "u16"
  | U32 -> "u32"
  | U64 -> "u64"
  | Usize -> "usize"

let encode_int_ty (enc : Encoder.t) int_ty =
  let disc = int_ty_to_enum int_ty |> Int64.of_int in
  Buffer.add_int64_be enc.buf disc

let decode_int_ty (dec : Decoder.t) =
  let dis = Decoder.read_usize dec in
  Option.get @@ int_ty_of_enum dis

let signed_range (n : int) : int * int =
  let n = float_of_int (n - 1) in
  let lo = Float.pow (-2.0) n in
  let hi = Float.pow 2.0 n in
  (int_of_float lo, int_of_float hi - 1)

let unsigned_range (n : int) : int * int =
  let n = float_of_int n in
  let hi = Float.pow 2.0 n in
  (0, int_of_float hi - 1)

let integer_ranges = function
  | I8 -> signed_range 8
  | U8 -> unsigned_range 8
  | I16 -> signed_range 16
  | U16 -> unsigned_range 16
  | I32 -> signed_range 32
  | U32 -> unsigned_range 32
  | I64 | Isize -> signed_range 64
  | U64 | Usize -> unsigned_range 64

let is_unsigned = function
  | U8 | U16 | U32 | U64 | Usize -> true
  | _ -> false

let is_signed = function I8 | I16 | I32 | I64 | Isize -> true | _ -> false

type float_ty =
  | F64
  | F32
[@@deriving enum]

let display_float_ty = function F32 -> "f32" | F64 -> "f64"

let encode_float_ty (enc : Encoder.t) float_ty =
  let disc = float_ty_to_enum float_ty |> Int64.of_int in
  Buffer.add_int64_be enc.buf disc

let decode_float_ty (dec : Decoder.t) =
  let dis = Decoder.read_usize dec in
  Option.get @@ float_ty_of_enum dis

type ty_vid = { index : int }

type infer_ty =
  | IntVar of ty_vid
  | FloatVar of ty_vid
  | TyVar of ty_vid

let i, f = (ref 0, ref 0)

let int_var_id () =
  let t = !i in
  incr i; t

let float_var_id () =
  let t = !f in
  incr f; t

let display_infer_ty = function
  | IntVar i -> sprintf "\x1b[1;31m?%di\x1b[0m" i.index
  | FloatVar i -> sprintf "\x1b[1;31m?%df\x1b[0m" i.index
  | TyVar i -> sprintf "\x1b[1;31m?%d\x1b[0m" i.index

let render_infer_ty ty dbg =
  if dbg then display_infer_ty ty
  else (
    match ty with
    | IntVar _ -> "int"
    | FloatVar _ -> "float"
    | TyVar _ -> "T")

type path_segment = string

type def_id = {
  inner : int;
  unit_id : int;
}

let def_id inner unit_id = { inner; unit_id }

let print_def_id def_id = sprintf "def_id#%d:%d" def_id.inner def_id.unit_id

type prim_ty =
  | Int of int_ty
  | Float of float_ty
  | Bool
  | Str

and def_kind =
  (* Type namespace *)
  | Mod
  | Struct
  (* Value namespace *)
  | Fn
  | Intrinsic

and res =
  | Def of (def_id * def_kind)
  | PrimTy of prim_ty
  | Local of int
  | Err

let def_kind_to_enum = function
  | Mod -> 0L
  | Struct -> 1L
  | Fn -> 2L
  | Intrinsic -> 3L

let def_kind_from_enum = function
  | 0 -> Mod
  | 1 -> Struct
  | 2 -> Fn
  | 3 -> Intrinsic
  | _ -> assert false

let res_to_enum = function
  | Def _ -> 0L
  | PrimTy _ -> 1L
  | Local _ -> 2L
  | Err -> 3L

let encode_res enc res =
  let dis = res_to_enum res in
  match res with
  | Def (def_id, def_kind) ->
      Encoder.emit_with enc dis (fun e ->
          Encoder.emit_u32 e def_id.inner;
          let disc = def_kind_to_enum def_kind in
          Buffer.add_int64_be enc.buf disc)
  | PrimTy _ -> assert false
  | Local _ -> assert false
  | Err -> assert false

let decode_res dec : res =
  let dis = Decoder.read_usize dec in
  match dis with
  | 0 ->
      let def_id = def_id (Decoder.read_u32 dec) dec.unit_id in
      let def_kind = def_kind_from_enum (Decoder.read_usize dec) in
      Def (def_id, def_kind)
  | 1 -> assert false
  | _ -> assert false

type path = {
  mutable segments : path_segment list;
  mutable res : res;
}

type ty =
  | Int of int_ty
  | Float of float_ty
  | Bool
  | Str
  | Ptr of ty
  | RefTy of ty
  | FnTy of ty list * ty * bool
  | Struct of string * (string * ty) list
  | Infer of infer_ty
  | Ident of path
  | Unit

let prim_ty_to_ty : prim_ty -> ty = function
  | Int int_ty -> Int int_ty
  | Float float_ty -> Float float_ty
  | Bool -> Bool
  | Str -> Str

let print_def_kind = function
  | Mod -> "mod"
  | Struct -> "struct"
  | Fn -> "fn"
  | Intrinsic -> "intrinsic"

let print_prim_ty : prim_ty -> string = function
  | Int _ -> "int"
  | Float _ -> "float"
  | Bool -> "bool"
  | Str -> "str"

let print_res : res -> string = function
  | Def (id, kind) ->
      sprintf "(%s~%s)" (print_def_kind kind) (print_def_id id)
  | PrimTy ty -> print_prim_ty ty
  | Local id -> "local#" ^ string_of_int id
  | Err -> "err"

let render (items : 'a list) (func : 'a -> string) (sep : string) : string =
  String.concat sep (List.map (fun item -> func item) items)

let render_path (path : path) : string = String.concat "::" path.segments

let rec render_ty ?(dbg = true) (ty : ty) : string =
  match ty with
  | Int ty -> display_int_ty ty
  | Float ty -> display_float_ty ty
  | Bool -> "bool"
  | Str -> "str"
  | Ptr ty -> sprintf "*%s" (render_ty ?dbg:(Some dbg) ty)
  | RefTy ty -> sprintf "&%s" (render_ty ?dbg:(Some dbg) ty)
  | Unit -> "()"
  | FnTy (ty_list, ret_ty, is_variadic) ->
      sprintf "fn(%s%s) -> %s"
        (render ty_list (fun ty -> render_ty ty) ", ")
        (if is_variadic then ", ..." else "")
        (render_ty ret_ty)
  | Struct (s, _) -> s
  | Ident path -> render_path path
  | Infer ty -> render_infer_ty ty dbg

type def_data = Ty of ty

let print_def_data = function Ty ty -> render_ty ty

let size_of_int = function
  | I8 | U8 -> 1
  | I16 | U16 -> 2
  | I32 | U32 -> 4
  | I64 | U64 | Isize | Usize -> 8

let size_of_float = function F32 -> 4 | F64 -> 8

let size_of = function
  | Int i -> size_of_int i
  | Float f -> size_of_float f
  | _ -> assert false

type common_types = {
  i8 : ty;
  i16 : ty;
  i32 : ty;
  i64 : ty;
  isize : ty;
  u8 : ty;
  u16 : ty;
  u32 : ty;
  u64 : ty;
  usize : ty;
  f32 : ty;
  f64 : ty;
  bool : ty;
  str : ty;
  unit : ty;
}

let common_types () =
  {
    i8 = Int I8;
    i16 = Int I16;
    i32 = Int I32;
    i64 = Int I64;
    isize = Int Isize;
    u8 = Int U8;
    u16 = Int U16;
    u32 = Int U32;
    u64 = Int U64;
    usize = Int Usize;
    f32 = Float F32;
    f64 = Float F64;
    bool = Bool;
    str = Str;
    unit = Unit;
  }

type def_table = {
  table : (def_id, def_data) Hashtbl.t;
  impls : (ty, (string, def_id) Hashtbl.t) Hashtbl.t;
}

let create_def def_table id data = Hashtbl.replace def_table.table id data

let create_impl def_table ty name impl =
  match Hashtbl.find_opt def_table.impls ty with
  | Some impls -> Hashtbl.replace impls name impl
  | None ->
      let tbl = Hashtbl.create 0 in
      Hashtbl.add tbl name impl;
      Hashtbl.add def_table.impls ty tbl

let lookup_def def_table id =
  if Hashtbl.mem def_table.table id then Hashtbl.find def_table.table id
  else assert false

let lookup_assoc_fn def_table ty name =
  match Hashtbl.find_opt def_table.impls ty with
  | Some tbl -> (
    match Hashtbl.find_opt tbl name with
    | Some id -> id
    | None -> assert false)
  | None -> assert false

let print_def_table def_table =
  let f (def_id, def_data) =
    sprintf "%s -> %s" (print_def_id def_id) (print_def_data def_data)
  in
  Hashtbl.to_seq def_table.table
  |> List.of_seq |> List.map f |> String.concat "\n"

type sym_table = (def_id, string) Hashtbl.t

let create_sym sym_table id name = Hashtbl.replace sym_table id name

let lookup_sym sym_table id =
  if Hashtbl.mem sym_table id then Hashtbl.find sym_table id
  else assert false

module Module = struct
  type t = {
    inner : llmodule;
    llcx : llcontext;
  }

  let create (sess : Sess.t) =
    let llcx = global_context () in
    let inner = create_module llcx sess.options.input in
    set_target_triple sess.target.triple inner;
    { inner; llcx }
end

type tcx = {
  sess : Sess.t;
  out_mod : Module.t;
  tys : common_types;
  lltys : llvm_types;
  def_table : def_table;
  sym_table : sym_table;
  sym_table2 : (string, def_id) Hashtbl.t;
  mutable units : string list;
  mutable uuid : int;
}

and llvm_types = {
  i8 : lltype;
  i16 : lltype;
  i32 : lltype;
  i64 : lltype;
  f32 : lltype;
  f64 : lltype;
  bool : lltype;
  ptr : lltype;
  size_type : lltype;
  void : lltype;
  structs : (string, lltype) Hashtbl.t;
}

let backend_types (target : Sess.target) (out_mod : Module.t) =
  let ctx = out_mod.llcx in
  {
    i8 = i8_type ctx;
    i16 = i16_type ctx;
    i32 = i32_type ctx;
    i64 = i64_type ctx;
    f32 = float_type ctx;
    f64 = double_type ctx;
    bool = i1_type ctx;
    ptr = pointer_type ctx;
    size_type = DataLayout.intptr_type ctx target.data_layout;
    void = void_type ctx;
    structs = Hashtbl.create 0;
  }

let tcx_create sess =
  let out_mod = Module.create sess in
  {
    sess;
    out_mod;
    tys = common_types ();
    lltys = backend_types sess.target out_mod;
    def_table = { table = Hashtbl.create 0; impls = Hashtbl.create 0 };
    sym_table = Hashtbl.create 0;
    sym_table2 = Hashtbl.create 0;
    units = [];
    uuid = 0;
  }

let tcx_gen_id tcx =
  tcx.uuid <- tcx.uuid + 1;
  tcx.uuid

let tcx_metadata tcx = Encoder.data tcx.sess.enc

let create_def tcx id def_data name =
  create_def tcx.def_table id def_data;
  match name with
  | Some name ->
      create_sym tcx.sym_table id name;
      create_sym tcx.sym_table2 name id
  | None -> ()

let create_impl tcx ty name impl = create_impl tcx.def_table ty name impl

let lookup_def tcx id = lookup_def tcx.def_table id

let expect_def tcx res expected : ty option =
  match res with
  | Def (id, kind) when kind = expected -> (
      let def = lookup_def tcx id in
      match def with Ty ty -> Some ty)
  | _ -> None

let lookup_sym2 tcx str : def_id = lookup_sym tcx.sym_table2 str

let lookup_sym tcx res : string =
  match res with
  | Def (id, _) -> lookup_sym tcx.sym_table id
  | _ -> assert false

let rec unwrap_ty tcx ty : ty =
  match ty with
  | Ident path -> (
    match path.res with
    | Def (id, _) -> ( lookup_def tcx id |> function Ty ty -> ty)
    | PrimTy ty -> prim_ty_to_ty ty
    | res ->
        print_endline (print_res res);
        assert false)
  | RefTy ty -> RefTy (unwrap_ty tcx ty)
  | Ptr ty -> Ptr (unwrap_ty tcx ty)
  | Int _ | Float _ | Bool | Str | Unit -> ty
  | Struct (name, fields) ->
      Struct (name, List.map (fun (f, ty) -> (f, unwrap_ty tcx ty)) fields)
  | FnTy (args, ty, is_var) ->
      FnTy
        (List.map (fun ty -> unwrap_ty tcx ty) args, unwrap_ty tcx ty, is_var)
  | Infer _ -> ty

let rec get_backend_type tcx' (ty : ty) : lltype =
  let ctx = tcx'.out_mod.llcx in
  let tcx = tcx'.lltys in
  match ty with
  | Float ty -> ( match ty with F32 -> tcx.f32 | F64 -> tcx.f64)
  | Bool -> tcx.bool
  | Str -> struct_type ctx [|pointer_type ctx; tcx.size_type|]
  | Int ty -> (
    match ty with
    | Isize | Usize -> tcx.size_type
    | I64 | U64 -> tcx.i64
    | I32 | U32 -> tcx.i32
    | I16 | U16 -> tcx.i16
    | I8 | U8 -> tcx.i8)
  | Ptr _ | RefTy _ | FnTy _ -> tcx.ptr
  | Struct (name, tys) ->
      if Hashtbl.mem tcx.structs name then Hashtbl.find tcx.structs name
      else (
        let ty = named_struct_type ctx name in
        struct_set_body ty
          (Array.of_list
             (List.map (fun (_, ty) -> get_backend_type tcx' ty) tys))
          false;
        Hashtbl.add tcx.structs name ty;
        ty)
  | Unit -> tcx.void
  | Ident path -> (
    match path.res with
    | Def (def_id, _) -> (
        lookup_def tcx' def_id
        |> function Ty ty -> ty |> get_backend_type tcx')
    | _ -> Hashtbl.find tcx.structs (render_path path))
  | Infer _ -> assert false

let discriminator = function
  | Int _ -> 0L
  | Float _ -> 1L
  | Bool -> 2L
  | Str -> 3L
  | Ptr _ -> 4L
  | RefTy _ -> 5L
  | FnTy _ -> 6L
  | Struct _ -> 7L
  | Infer _ -> 8L
  | Ident _ -> 9L
  | Unit -> 10L

let rec encode enc ty =
  let dis = discriminator ty in
  ty
  |> function
  | Int i -> Encoder.emit_with enc dis (fun e -> encode_int_ty e i)
  | Float f -> Encoder.emit_with enc dis (fun e -> encode_float_ty e f)
  | FnTy (args, ret_ty, is_var) ->
      Encoder.emit_with enc dis (fun e ->
          Encoder.emit_u32 e (List.length args);
          List.iter (fun ty -> encode e ty) args;
          encode e ret_ty;
          Encoder.emit_u8 e (if is_var then 1 else 0))
  | Bool | Unit | Str -> Encoder.emit_with enc dis (fun _ -> ())
  | Ptr ty | RefTy ty -> Encoder.emit_with enc dis (fun e -> encode e ty)
  | Struct (name, fields) ->
      Encoder.emit_with enc dis (fun e ->
          Encoder.emit_str e name;
          Encoder.emit_u32 e (List.length fields);
          List.iter
            (fun (field, ty) -> Encoder.emit_str e field; encode e ty)
            fields)
  | Ident path ->
      Encoder.emit_with enc dis (fun e ->
          Encoder.emit_u32 e (List.length path.segments);
          List.iter (fun s -> Encoder.emit_str e s) path.segments;
          encode_res e path.res)
  | Infer _ -> assert false

let encode_metadata tcx =
  let enc = tcx.sess.enc in
  Encoder.emit_usize enc @@ List.length tcx.units;
  List.iter (fun name -> Encoder.emit_str enc name) tcx.units;
  Encoder.emit_usize enc @@ Hashtbl.length tcx.def_table.table;
  Hashtbl.iter
    (fun def_id def_data ->
      Encoder.emit_u32 enc def_id.inner;
      def_data |> function Ty ty -> encode enc ty)
    tcx.def_table.table;
  Encoder.emit_usize enc @@ Hashtbl.length tcx.sym_table;
  Hashtbl.iter
    (fun def_id sym ->
      Encoder.emit_str enc sym;
      Encoder.emit_u32 enc def_id.inner)
    tcx.sym_table

let rec decode dec =
  let dis = Decoder.read_usize dec in
  match dis with
  | 0 -> Int (decode_int_ty dec)
  | 1 -> Float (decode_float_ty dec)
  | 2 -> Bool
  | 3 -> Str
  | 4 -> Ptr (decode dec)
  | 5 -> RefTy (decode dec)
  | 6 ->
      let nargs = Decoder.read_u32 dec in
      let args =
        List.map (fun _ -> decode dec) (List.init nargs (fun x -> x))
      in
      let ret_ty = decode dec in
      let is_var = Decoder.read_u8 dec = 1 in
      FnTy (args, ret_ty, is_var)
  | 7 ->
      let name = Decoder.read_str dec in
      let nfields = Decoder.read_u32 dec in
      let fields =
        List.map
          (fun _ ->
            let field = Decoder.read_str dec in
            let ty = decode dec in
            (field, ty))
          (List.init nfields (fun x -> x))
      in
      Struct (name, fields)
  | 8 -> assert false (* Infer *)
  | 9 ->
      let nsegments = Decoder.read_u32 dec in
      let segments =
        List.map
          (fun _ -> Decoder.read_str dec)
          (List.init nsegments (fun x -> x))
      in
      let res = decode_res dec in
      Ident { segments; res }
  | 10 -> Unit
  | _ ->
      printf "%Ld: %d\n" (dis |> Int64.of_int) dec.pos;
      assert false

let decode_metadata tcx dec =
  let nunits = Decoder.read_usize dec in
  let units =
    List.map (fun _ -> Decoder.read_str dec) (List.init nunits (fun x -> x))
  in
  tcx.units <- tcx.units @ units;
  let ndef_table_entries = Decoder.read_usize dec in
  List.iter
    (fun _ ->
      let def_id = def_id (Decoder.read_u32 dec) dec.unit_id in
      let ty = decode dec in
      create_def tcx def_id (Ty ty) None)
    (List.init ndef_table_entries (fun x -> x));
  let nsym_table_entries = Decoder.read_usize dec in
  List.iter
    (fun _ ->
      let sym = Decoder.read_str dec in
      let def_id = def_id (Decoder.read_u32 dec) dec.unit_id in
      create_sym tcx.sym_table def_id sym;
      create_sym tcx.sym_table2 sym def_id)
    (List.init nsym_table_entries (fun x -> x))

let ty_eq tcx t1 t2 : bool =
  match (t1, t2) with
  | Ident p1, Ident p2 -> p1.res = p2.res
  | Ident path, (Struct _ as t) | (Struct _ as t), Ident path -> (
    match expect_def tcx path.res Struct with
    | Some ty -> ty = t
    | None -> false)
  | _ -> t1 = t2

let ty_neq tcx t1 t2 = not @@ ty_eq tcx t1 t2
