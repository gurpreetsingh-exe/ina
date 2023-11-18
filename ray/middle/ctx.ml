open Ty
open Session
open Printf
open Structures.Hashmap
open Def_id
open Utils.Panic
open Source

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
  }
;;

class tcx sess =
  object (self)
    val types : (ty, ty ref) hashmap = new hashmap
    val node_id_to_ty : ty ref nodemap = new hashmap
    method node_id_to_ty = node_id_to_ty
    val res_map : res nodemap = new hashmap
    method res_map = res_map
    val spans : Span.t nodemap = new hashmap
    method spans = spans
    val sess : Sess.t = sess
    val mutable _types = dummy_types

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
        }
      in
      _types <- ty

    method sess = sess
    method types = _types

    method intern ty : ty ref =
      if types#has ty
      then types#unsafe_get ty
      else (
        dbg "intern(type = %s)\n" @@ render_ty ty;
        let rty = ref ty in
        ignore (types#insert ty rty);
        rty)

    method invalidate old_ty new_ty =
      let ty = self#intern old_ty in
      dbg
        "invalidate(old = %s, new = %s)\n"
        (render_ty old_ty)
        (render_ty new_ty);
      ty := new_ty

    method emit err = Sess.emit_err sess.parse_sess err

    method insert_span id span =
      dbg "tcx.insert_span(id = %d, span = %s)\n" id (Span.display_span span);
      assert (spans#insert id span = None)

    method ast_ty_to_ty (ty : Ast.ty) : ty ref =
      ref
      @@
      match ty.kind with
      | Int i ->
          Ty.Int
            (match i with
             | I8 -> I8
             | I16 -> I16
             | I32 -> I32
             | I64 -> I64
             | Isize -> Isize
             | U8 -> U8
             | U16 -> U16
             | U32 -> U32
             | U64 -> U64
             | Usize -> Usize)
      | _ -> assert false
  end
