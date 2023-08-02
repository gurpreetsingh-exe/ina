open Printf
open Session

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

let display_float_ty = function F32 -> "f32" | F64 -> "f64"

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

type def_id = { inner : int }

let def_id inner = { inner }

let print_def_id def_id = "def_id#" ^ string_of_int def_id.inner

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

and res =
  | Def of (def_id * def_kind)
  | PrimTy of prim_ty
  | Local of int
  | Err

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

let rec ( != ) (ty1 : ty) (ty2 : ty) : bool =
  match (ty1, ty2) with
  | Ident path, Struct (name, _) | Struct (name, _), Ident path ->
      String.concat "::" path.segments <> name
  | Ident path1, Ident path2 ->
      String.concat "::" path1.segments <> String.concat "::" path2.segments
  | Struct (name1, fields1), Struct (name2, fields2) ->
      let tys1 = List.map (fun (_, ty) -> ty) fields1 in
      let tys2 = List.map (fun (_, ty) -> ty) fields2 in
      (if List.length tys1 = 0 && List.length tys2 = 0 then false
      else (
        try
          List.map2 ( != ) tys1 tys2
          |> List.filter (fun f -> not f)
          |> List.length = 0
        with Invalid_argument _ -> true))
      || name1 <> name2
  | Ptr ty1, Ptr ty2 -> ty1 != ty2
  | RefTy ty1, RefTy ty2 -> ty1 != ty2
  | FnTy (tys1, ret_ty1, is_var1), FnTy (tys2, ret_ty2, is_var2) ->
      (if List.length tys1 = 0 && List.length tys2 = 0 then false
      else (
        try
          List.map2 ( != ) tys1 tys2
          |> List.filter (fun f -> not f)
          |> List.length = 0
        with Invalid_argument _ -> true))
      || ret_ty1 != ret_ty2 || is_var1 <> is_var2
  | _, _ -> ty1 <> ty2

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

type def_table = { table : (def_id, def_data) Hashtbl.t }

let create_def def_table id data = Hashtbl.replace def_table.table id data

let lookup_def def_table id =
  if Hashtbl.mem def_table.table id then Hashtbl.find def_table.table id
  else assert false

let print_def_table def_table =
  let f (def_id, def_data) =
    sprintf "%s -> %s" (print_def_id def_id) (print_def_data def_data)
  in
  Hashtbl.to_seq def_table.table
  |> List.of_seq |> List.map f |> String.concat "\n"

type sym_table = (def_id, string) Hashtbl.t

let mangle path =
  "_Z"
  ^ String.concat ""
      (List.map
         (fun seg -> string_of_int (String.length seg) ^ seg)
         path.segments)

let create_sym sym_table id path = Hashtbl.replace sym_table id (mangle path)

let lookup_sym sym_table id =
  if Hashtbl.mem sym_table id then Hashtbl.find sym_table id
  else assert false

type tcx = {
  sess : Sess.t;
  tys : common_types;
  def_table : def_table;
  sym_table : sym_table;
  mutable uuid : int;
}

let tcx_create sess =
  {
    sess;
    tys = common_types ();
    def_table = { table = Hashtbl.create 0 };
    sym_table = Hashtbl.create 0;
    uuid = 0;
  }

let tcx_gen_id tcx =
  tcx.uuid <- tcx.uuid + 1;
  tcx.uuid

let create_def tcx id def_data path =
  create_def tcx.def_table id def_data;
  create_sym tcx.sym_table id path

let lookup_def tcx id = lookup_def tcx.def_table id

let expect_def tcx res expected : ty option =
  match res with
  | Def (id, kind) when kind = expected -> (
      let def = lookup_def tcx id in
      match def with Ty ty -> Some ty)
  | _ -> None

let lookup_sym tcx res : string =
  match res with
  | Def (id, _) -> lookup_sym tcx.sym_table id
  | _ -> assert false
