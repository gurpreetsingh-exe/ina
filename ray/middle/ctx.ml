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
    i8: ty
  ; i16: ty
  ; i32: ty
  ; i64: ty
  ; isize: ty
  ; u8: ty
  ; u16: ty
  ; u32: ty
  ; u64: ty
  ; usize: ty
  ; f32: ty
  ; f64: ty
  ; bool: ty
  ; str: ty
  ; unit: ty
}

let dummy_types =
  {
    i8 = Unit
  ; i16 = Unit
  ; i32 = Unit
  ; i64 = Unit
  ; isize = Unit
  ; u8 = Unit
  ; u16 = Unit
  ; u32 = Unit
  ; u64 = Unit
  ; usize = Unit
  ; f32 = Unit
  ; f64 = Unit
  ; bool = Unit
  ; str = Unit
  ; unit = Unit
  }
;;

class tcx sess =
  object (self)
    val types : (ty, unit) hashmap = new hashmap
    val node_id_to_ty : ty nodemap = new hashmap
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
          i8 = Int I8
        ; i16 = Int I16
        ; i32 = Int I32
        ; i64 = Int I64
        ; isize = Int Isize
        ; u8 = Int U8
        ; u16 = Int U16
        ; u32 = Int U32
        ; u64 = Int U64
        ; usize = Int Usize
        ; f32 = Float F32
        ; f64 = Float F64
        ; bool = Bool
        ; str = Str
        ; unit = Unit
        }
      in
      ignore (self#intern ty.i8);
      ignore (self#intern ty.i16);
      ignore (self#intern ty.i32);
      ignore (self#intern ty.i64);
      ignore (self#intern ty.isize);
      ignore (self#intern ty.u8);
      ignore (self#intern ty.u16);
      ignore (self#intern ty.u32);
      ignore (self#intern ty.u64);
      ignore (self#intern ty.usize);
      ignore (self#intern ty.f32);
      ignore (self#intern ty.f64);
      ignore (self#intern ty.bool);
      ignore (self#intern ty.str);
      ignore (self#intern ty.unit);
      _types <- ty

    method sess = sess
    method types = _types

    method intern ty : ty =
      if not @@ types#has ty then ignore (types#insert ty ());
      dbg "intern(type = %s)\n" @@ render_ty ty;
      ty

    method emit err = Sess.emit_err sess.parse_sess err

    method insert_span id span =
      dbg "tcx.insert_span(id = %d, span = %s)\n" id (Span.display_span span);
      assert (spans#insert id span = None)

    method ast_ty_to_ty (ty : Ast.ty) : ty =
      match ty.kind with
      | Int i ->
          Int
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
