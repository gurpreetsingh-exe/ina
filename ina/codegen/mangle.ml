open Printf
open Ir.Inst
open Middle.Ctx
open Middle.Ty

let mangle_mut = function Mut -> "M" | _ -> ""

let rec mangle_ty (tcx : tcx) ty =
  match !ty with
  | Param { name; _ } -> name
  | Adt (did, Subst subst') ->
      let segments, _ =
        tcx#into_segments ~f:(fun ty -> mangle_ty tcx ty) did
      in
      let args =
        if subst'#empty
        then String.empty
        else
          String.concat
            ""
            [
              "$LB"
            ; subst'#join "" (function Ty ty -> "$" ^ mangle_ty tcx ty)
            ; "$RB"
            ]
      in
      String.concat "_" segments ^ args
  | Int i ->
      i |> ( function
      | I8 -> "a"
      | I16 -> "b"
      | I32 -> "c"
      | I64 -> "d"
      | Isize -> "e"
      | U8 -> "A"
      | U16 -> "B"
      | U32 -> "C"
      | U64 -> "D"
      | Usize -> "E" )
  | Float F32 -> "f"
  | Float F64 -> "F"
  | Bool -> "g"
  | Str -> "s"
  | Slice ty -> "S" ^ mangle_ty tcx ty
  | Unit -> "u"
  | Ptr (m, ty) -> "p" ^ mangle_mut m ^ mangle_ty tcx ty
  | Ref (m, ty) -> "r" ^ mangle_mut m ^ mangle_ty tcx ty
  | _ ->
      tcx#render_ty ty |> print_endline;
      assert false
;;

let mangle_def_path (tcx : tcx) did =
  let segments, extern =
    tcx#into_segments ~f:(fun ty -> mangle_ty tcx ty) did
  in
  match extern with
  | true -> List.nth segments (List.length segments - 1)
  | false ->
      let name =
        segments
        |> List.map (fun s -> sprintf "%d%s" (String.length s) s)
        |> String.concat ""
      in
      String.concat "" ["_IN"; name]
;;

let mangle (tcx : tcx) instance =
  let { def = Fn did | Intrinsic did; subst = Subst subst } = instance in
  let segments, extern =
    tcx#into_segments ~f:(fun ty -> mangle_ty tcx ty) did
  in
  match extern with
  | true -> List.nth segments (List.length segments - 1)
  | false ->
      String.concat
        ""
        [
          "_IN"
        ; segments
          |> List.map (fun s -> sprintf "%d%s" (String.length s) s)
          |> String.concat ""
        ; (if subst#empty
           then ""
           else subst#join "" (function Ty ty -> mangle_ty tcx ty))
        ]
;;
