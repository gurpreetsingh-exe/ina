open Ty
open Session
open Structures.Vec
open Structures.Hashmap
open Def_id
open Utils.Panic
open Source
open Printf
module TypeMap = Hashtbl.Make (Ty)

type 'a nodemap = (int, 'a) hashmap

type prim_ty =
  | Int of int_ty
  | Float of float_ty
  | Bool
  | Str

and res =
  | Def of (def_id * def_kind)
  | PrimTy of prim_ty
  | Local of int
  | Err

let print_prim_ty : prim_ty -> string = function
  | Int _ -> "int"
  | Float _ -> "float"
  | Bool -> "bool"
  | Str -> "str"
;;

let print_res : res -> string = function
  | Def (id, kind) ->
      sprintf "(%s~%s)" (print_def_kind kind) (print_def_id id)
  | PrimTy ty -> print_prim_ty ty
  | Local id -> "local#" ^ string_of_int id
  | Err -> "err"
;;

type types = {
    i8: ty ref
  ; i16: ty ref
  ; i32: ty ref
  ; i64: ty ref
  ; isize: ty ref
  ; u8: ty ref
  ; u16: ty ref
  ; u32: ty ref
  ; u64: ty ref
  ; usize: ty ref
  ; f32: ty ref
  ; f64: ty ref
  ; bool: ty ref
  ; str: ty ref
  ; unit: ty ref
  ; err: ty ref
}

let dummy_types =
  {
    i8 = ref Unit
  ; i16 = ref Unit
  ; i32 = ref Unit
  ; i64 = ref Unit
  ; isize = ref Unit
  ; u8 = ref Unit
  ; u16 = ref Unit
  ; u32 = ref Unit
  ; u64 = ref Unit
  ; usize = ref Unit
  ; f32 = ref Unit
  ; f64 = ref Unit
  ; bool = ref Unit
  ; str = ref Unit
  ; unit = ref Unit
  ; err = ref Unit
  }
;;

class tcx sess =
  object (self)
    val types : ty ref TypeMap.t = TypeMap.create 0
    val node_id_to_ty : ty ref nodemap = new hashmap
    val node_id_to_def_id : def_id nodemap = new hashmap
    val res_map : res nodemap = new hashmap
    val def_id_to_qpath : (def_id, string vec) hashmap = new hashmap
    val spans : Span.t nodemap = new hashmap
    val sess : Sess.t = sess
    val mutable main : def_id option = None
    val mutable _types = dummy_types
    val mutable err_count = 0
    val extern_decls : (string, def_id) hashmap = new hashmap
    val extern_def_ids : (def_id, unit) hashmap = new hashmap
    val adt_def : (def_id, adt) hashmap = new hashmap

    initializer
      let ty =
        {
          i8 = self#intern (Ty.Int I8)
        ; i16 = self#intern (Int I16)
        ; i32 = self#intern (Int I32)
        ; i64 = self#intern (Int I64)
        ; isize = self#intern (Int Isize)
        ; u8 = self#intern (Int U8)
        ; u16 = self#intern (Int U16)
        ; u32 = self#intern (Int U32)
        ; u64 = self#intern (Int U64)
        ; usize = self#intern (Int Usize)
        ; f32 = self#intern (Float F32)
        ; f64 = self#intern (Float F64)
        ; bool = self#intern Bool
        ; str = self#intern Str
        ; unit = self#intern Unit
        ; err = self#intern Err
        }
      in
      _types <- ty

    method sess = sess
    method types = _types
    method node_id_to_ty = node_id_to_ty
    method node_id_to_def_id = node_id_to_def_id
    method res_map = res_map
    method def_id_to_qpath = def_id_to_qpath
    method spans = spans
    method extern_decls = extern_decls
    method set_main id = main <- Some id
    method main = main
    method is_extern did = extern_def_ids#has did

    method decl_extern name did =
      extern_def_ids#insert' did ();
      match extern_decls#get name with
      | Some did -> did
      | None ->
          assert (extern_decls#insert name did = None);
          did

    method intern ty : ty ref =
      match TypeMap.find_opt types ty with
      | Some ty -> ty
      | None ->
          let rty = ref ty in
          dbg "intern(type = %s)\n" @@ render_ty2 rty;
          ignore (TypeMap.add types ty rty);
          rty

    method invalidate old_ty new_ty =
      let ty = self#intern old_ty in
      if ty <> new_ty
      then (
        dbg
          "invalidate(old = %s, new = %s)\n"
          (render_ty2 ty)
          (render_ty2 new_ty);
        ty := !new_ty)

    method emit err =
      err_count <- err_count + 1;
      Sess.emit_err sess.parse_sess err

    method has_errors = err_count > 0

    method insert_span id span =
      dbg "tcx.insert_span(id = %d, span = %s)\n" id (Span.display_span span);
      assert (spans#insert id span = None)

    method ptr ty = self#intern (Ptr ty)
    method ref ty = self#intern (Ref ty)

    method fn_ptr args ret is_variadic abi =
      self#intern (FnPtr { args; ret; is_variadic; abi })

    method adt def_id = self#intern (Adt def_id)

    method adt_with_variants def_id variants =
      adt_def#insert' def_id { variants };
      self#adt def_id

    method sizeof_int_ty =
      function
      | I8 | U8 -> 1
      | U16 | I16 -> 2
      | U32 | I32 -> 4
      | U64 | Usize | I64 | Isize -> 8

    method sizeof_float_ty = function F32 -> 4 | F64 -> 8

    method sizeof ty =
      match !ty with
      | Ty.Int i -> self#sizeof_int_ty i
      | Float f -> self#sizeof_float_ty f
      | Ptr _ | Ref _ | FnPtr _ -> 8
      | Str -> 16
      | Unit -> 0
      | Bool -> 1
      | _ -> assert false

    method non_enum_variant ty =
      match !ty with
      | Adt def_id ->
          let variants = (adt_def#unsafe_get def_id).variants in
          assert (variants#len = 1);
          variants#get 0
      | _ -> assert false

    method int_ty_to_ty =
      function
      | I8 -> _types.i8
      | I16 -> _types.i16
      | I32 -> _types.i32
      | I64 -> _types.i64
      | Isize -> _types.isize
      | U8 -> _types.u8
      | U16 -> _types.u16
      | U32 -> _types.u32
      | U64 -> _types.u64
      | Usize -> _types.usize

    method float_ty_to_ty = function F32 -> _types.f32 | F64 -> _types.f64

    method ast_int_ty_to_ty : Ast.int_ty -> ty ref =
      function
      | I8 -> _types.i8
      | I16 -> _types.i16
      | I32 -> _types.i32
      | I64 -> _types.i64
      | Isize -> _types.isize
      | U8 -> _types.u8
      | U16 -> _types.u16
      | U32 -> _types.u32
      | U64 -> _types.u64
      | Usize -> _types.usize

    method ast_float_ty_to_ty : Ast.float_ty -> ty ref =
      function F32 -> _types.f32 | F64 -> _types.f64

    method ast_ty_to_ty (ty : Ast.ty) : ty ref =
      match ty.kind with
      | Int i -> self#ast_int_ty_to_ty i
      | Float f -> self#ast_float_ty_to_ty f
      | Ptr ty -> self#intern (Ptr (self#ast_ty_to_ty ty))
      | Ref ty -> self#intern (Ref (self#ast_ty_to_ty ty))
      | Str -> _types.str
      | Bool -> _types.bool
      | Unit -> _types.unit
      | FnPtr (args, ret, is_variadic) ->
          self#intern
            (FnPtr
               {
                 args = map args self#ast_ty_to_ty
               ; ret = self#ast_ty_to_ty ret
               ; is_variadic
               ; abi = Default
               })
      | Err -> assert false
      | Path path ->
          let res = res_map#unsafe_get path.path_id in
          res |> ( function
          | Def (def_id, _) -> self#adt def_id
          | _ -> assert false )
      | _ -> assert false

    method inner_ty ty : ty ref option =
      match !ty with
      | Ptr ty | Ref ty | FnPtr { ret = ty; _ } -> Some ty
      | _ -> None

    method render_ty ty =
      match !ty with
      | Ty.Int i -> display_int_ty i
      | Float f -> display_float_ty f
      | Infer i ->
          (match i with
           | IntVar _ -> "{int}"
           | FloatVar _ -> "{float}"
           | TyVar _ -> "infer")
      | Unit -> "unit"
      | Bool -> "bool"
      | Str -> "str"
      | FnPtr { args; ret; is_variadic; abi } ->
          sprintf
            "%sfn(%s) -> %s"
            (abi |> function
             | Default -> ""
             | Intrinsic -> "\"intrinsic\" "
             | C -> "\"C\" ")
            (args#join ", " (fun ty -> self#render_ty ty)
             ^ if is_variadic then ", ..." else String.empty)
            (self#render_ty ret)
      | Ptr ty -> "*" ^ self#render_ty ty
      | Ref ty -> "&" ^ self#render_ty ty
      | Err -> "err"
      | Adt def_id ->
          let segments = def_id_to_qpath#unsafe_get def_id in
          segments#join "::" (fun s -> s)
  end
