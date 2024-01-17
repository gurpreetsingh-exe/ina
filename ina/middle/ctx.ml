open Ty
open Session
open Structures.Vec
open Structures.Hashmap
open Def_id
open Utils.Panic
open Source
open Printf
open Metadata
open Encoder
open Decoder
module TypeMap = Hashtbl.Make (Ty)

type def_data =
  | ModRoot
  | ExternMod
  | Impl of def_id
  | TypeNs of string
  | ValueNs of string

let def_data_discriminant = function
  | ModRoot -> 1
  | ExternMod -> 2
  | Impl _ -> 3
  | TypeNs _ -> 4
  | ValueNs _ -> 5
;;

let encode_def_data enc data : unit =
  let disc = def_data_discriminant data |> Int64.of_int in
  match data with
  | ModRoot | ExternMod -> enc#emit_with disc (fun _ -> ())
  | Impl id -> enc#emit_with disc (fun e -> Def_id.encode e id)
  | TypeNs name | ValueNs name ->
      enc#emit_with disc (fun e -> e#emit_str name)
;;

let decode_def_data dec =
  match dec#read_usize with
  | 1 -> ModRoot
  | 2 -> ExternMod
  | 3 -> Impl (Def_id.decode dec)
  | 4 -> TypeNs dec#read_str
  | 5 -> ValueNs dec#read_str
  | _ -> assert false
;;

module DefKey = struct
  type t = {
      parent: def_id option
    ; data: def_data
  }

  let encode enc key =
    (match key.parent with
     | Some parent ->
         enc#emit_usize 1;
         Def_id.encode enc parent
     | None -> enc#emit_usize 2);
    encode_def_data enc key.data
  ;;

  let decode dec =
    let parent =
      match dec#read_usize with
      | 1 -> Some (Def_id.decode dec)
      | 2 -> None
      | _ -> assert false
    in
    let data = decode_def_data dec in
    { parent; data }
  ;;

  let compute_hash parent k =
    let hasher = { hash = 0 } in
    fx_add_to_hash hasher parent;
    let disc = def_data_discriminant k.data in
    fx_add_to_hash hasher disc;
    (match k.data with
     | TypeNs name | ValueNs name ->
         fx_add_to_hash hasher (Hashtbl.hash name)
     | _ -> ());
    hasher.hash
  ;;

  let print_def_data = function
    | ModRoot -> "ModRoot"
    | ExternMod -> "ExternMod"
    | Impl i -> "Impl " ^ print_def_id i
    | TypeNs name -> "TypeNs " ^ name
    | ValueNs name -> "ValueNs " ^ name
  ;;

  let display { parent; data } =
    sprintf
      "{ parent = %s, data = %s }"
      (match parent with Some p -> print_def_id p | None -> "None")
      (print_def_data data)
  ;;
end

module SubstFolder = struct
  let rec fold_ty tcx ty subst =
    match !ty with
    | Param { index; _ } -> subst#get index |> ( function Ty ty -> ty )
    | Ptr ty -> tcx#ptr (fold_ty tcx ty subst)
    | Ref ty -> tcx#ref (fold_ty tcx ty subst)
    | FnPtr fnsig ->
        let { args; ret; is_variadic; abi } = fold_fnsig tcx fnsig subst in
        tcx#fn_ptr args ret is_variadic abi
    | Fn (did, Subst subst') ->
        tcx#fn did (fold_subst tcx subst' subst)
    | Adt (did, Subst subst') ->
        tcx#adt did (fold_subst tcx subst' subst)
    | _ -> ty

  and fold_fnsig tcx { args; ret; is_variadic; abi } subst =
    let args = map args (fun ty -> fold_ty tcx ty subst) in
    let ret = fold_ty tcx ret subst in
    { args; ret; is_variadic; abi }

  and fold_field tcx (Field { ty; name }) subst =
    Field { ty = fold_ty tcx ty subst; name }

  and fold_variant tcx (Variant variant) subst =
    Variant
      {
        variant with
        fields = map variant.fields (fun f -> fold_field tcx f subst)
      }

  and fold_adt tcx adt subst =
    { variants = map adt.variants (fun v -> fold_variant tcx v subst) }

  and fold_subst tcx subst subst' =
    Subst (map subst (function Ty ty -> Ty (fold_ty tcx ty subst')))
  ;;
end

type 'a nodemap = (int, 'a) hashmap

and res =
  | Def of (def_id * def_kind)
  | Ty of ty ref
  | Local of int
  | Err

let encode_res enc res =
  match res with
  | Def (id, kind) ->
      enc#emit_with 0L (fun e ->
          Def_id.encode e id;
          Def_id.def_kind_to_enum kind |> e#emit_usize)
  | _ -> assert false
;;

let decode_res dec =
  match dec#read_usize with
  | 0 ->
      let id = Def_id.decode dec in
      let kind = Option.get @@ Def_id.def_kind_of_enum dec#read_usize in
      Def (id, kind)
  | _ -> assert false
;;

let print_res : res -> string = function
  | Def (id, kind) ->
      sprintf "(%s~%s)" (print_def_kind kind) (print_def_id id)
  | Ty ty -> render_ty ty
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
    val def_id_to_ty : (def_id, ty ref) hashmap = new hashmap
    val node_id_to_def_id : def_id nodemap = new hashmap
    val res_map : res nodemap = new hashmap
    val spans : Span.t nodemap = new hashmap
    val sess : Sess.t = sess
    val mutable main : def_id option = None
    val mutable _types = dummy_types
    val mutable err_count = 0
    val adt_def : (def_id, adt) hashmap = new hashmap
    val fn_def : (def_id, fnsig) hashmap = new hashmap
    val assoc_fn : (def_id, (string, def_id) hashmap) hashmap = new hashmap
    val extern_mods : string vec = new vec
    val definitions : (def_id, DefKey.t) hashmap = new hashmap
    val impls : (def_id, ty ref) hashmap = new hashmap
    val mutable impl_id = 0

    val prim_ty_assoc_fn : (ty ref, (string, def_id) hashmap) hashmap =
      new hashmap

    val extmods : (string, int) hashmap = new hashmap

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
    method node_id_to_def_id = node_id_to_def_id
    method res_map = res_map
    method spans = spans
    method set_main id = main <- Some id
    method main = main

    method is_extern did =
      let DefKey.{ parent; _ } = self#def_key did in
      let parent = Option.get parent in
      match self#def_key parent with
      | { data = ExternMod; _ } -> true
      | _ -> false

    method extmods = extmods
    method extern_mods = extern_mods
    method append_extern_mod name = extern_mods#push name

    method impl_id =
      let id = impl_id in
      impl_id <- impl_id + 1;
      id

    method encode_metadata =
      let enc = sess.enc in
      encode_vec enc extern_mods (fun e s -> e#emit_str s);
      encode_hashmap enc def_id_to_ty Def_id.encode Ty.encode;
      encode_hashmap enc adt_def Def_id.encode (fun e adt ->
          encode_vec e adt.variants Ty.encode_variant);
      encode_hashmap enc fn_def Def_id.encode Fn.encode;
      encode_hashmap enc assoc_fn Def_id.encode (fun e methods ->
          encode_hashmap e methods (fun e s -> e#emit_str s) Def_id.encode);
      encode_hashmap enc prim_ty_assoc_fn Ty.encode (fun e methods ->
          encode_hashmap e methods (fun e s -> e#emit_str s) Def_id.encode);
      encode_hashmap enc definitions Def_id.encode DefKey.encode;
      encode_hashmap enc impls Def_id.encode Ty.encode

    method decode_metadata (dec : decoder) =
      decode_vec dec extern_mods (fun dec -> dec#read_str);
      decode_hashmap dec def_id_to_ty Def_id.decode (fun dec ->
          Ty.decode self dec);
      decode_hashmap dec adt_def Def_id.decode (fun dec ->
          let variants = new vec in
          decode_vec dec variants (self |> Ty.decode_variant);
          { variants });
      decode_hashmap dec fn_def Def_id.decode (self |> decode_fn);
      decode_hashmap dec assoc_fn Def_id.decode (fun dec ->
          let methods = new hashmap in
          decode_hashmap dec methods (fun dec -> dec#read_str) Def_id.decode;
          methods);
      decode_hashmap dec prim_ty_assoc_fn (self |> Ty.decode) (fun dec ->
          let methods = new hashmap in
          decode_hashmap dec methods (fun dec -> dec#read_str) Def_id.decode;
          methods);
      decode_hashmap dec definitions Def_id.decode DefKey.decode;
      decode_hashmap dec impls Def_id.decode (self |> Ty.decode)

    method unit name =
      match extmods#get name with
      | Some id -> id
      | None ->
          let id = extmods#len + 1 in
          extmods#insert' name id;
          id

    method intern ty : ty ref =
      match TypeMap.find_opt types ty with
      | Some ty -> ty
      | None ->
          let rty = ref ty in
          dbg "intern(type = %s)\n" @@ render_ty2 rty;
          ignore (TypeMap.add types ty rty);
          rty

    method define parent id data =
      assert (data <> ModRoot);
      dbg
        "define(parent = %s, id = %s, def_data = %s)\n"
        (print_def_id parent)
        (print_def_id id)
        (DefKey.print_def_data data);
      let key = DefKey.{ parent = Some parent; data } in
      (match definitions#insert id key with
       | Some key' ->
           printf "%s = %s\n" (DefKey.display key') (DefKey.display key);
           assert (key' = key)
       | None -> ());
      id

    method define_root id =
      let key = DefKey.{ parent = None; data = ModRoot } in
      assert (definitions#insert id key = None);
      id

    method def_path id =
      (* printf "%s\n%!" (print_def_id id); *)
      let DefKey.{ parent; data } = definitions#unsafe_get id in
      match parent with
      | Some id ->
          let path = self#def_path id in
          path @ [data]
      | None -> [data]

    method def_key id = definitions#unsafe_get id

    method print_def_keys =
      let nodes = new hashmap in
      definitions#iter (fun did key ->
          match key.parent with
          | Some parent ->
              nodes#insert' parent (new vec);
              (nodes#unsafe_get parent)#push did
          | None -> ());
      let open Utils.Printer in
      let printer = new printer in
      render_children printer nodes (fun (k, v) prefix ->
          printer#append @@ DefKey.print_def_data (self#def_key k).data;
          printer#append "\n";
          render_children ?prefix:(Some prefix) printer v (fun i _ ->
              printer#append @@ DefKey.print_def_data (self#def_key i).data;
              printer#append "\n"));
      printer#print

    method into_segments id =
      let DefKey.{ parent; _ } = self#def_key id in
      let parent = Option.get parent in
      let extern =
        match self#def_key parent with
        | { data = ExternMod; _ } -> true
        | _ -> false
      in
      ( self#def_path id
        |> (function ModRoot :: rest -> rest | _ -> assert false)
        |> List.filter (function
               | TypeNs _ | ValueNs _ | Impl _ -> true
               | _ -> false)
        |> List.map (function
               | ModRoot | ExternMod -> assert false
               | Impl id -> self#render_ty (impls#unsafe_get id)
               | TypeNs name | ValueNs name -> name)
      , extern )

    method link_impl id ty = assert (impls#insert id ty = None)

    method create_def id ty =
      match def_id_to_ty#insert id ty with
      | Some ty' -> assert (ty = ty')
      | None -> ()

    method get_def id =
      match def_id_to_ty#get id with Some ty -> ty | None -> assert false

    method iter_infer_vars f =
      def_id_to_ty#iter (fun _ v ->
          match !v with Infer tyvar -> f (v, tyvar) | _ -> ())

    method define_assoc_fn res name fn =
      match res with
      | Def (id, (Struct | Impl)) ->
          assoc_fn#insert' id (new hashmap);
          (assoc_fn#unsafe_get id)#insert' name fn
      | Ty ty ->
          prim_ty_assoc_fn#insert' ty (new hashmap);
          (prim_ty_assoc_fn#unsafe_get ty)#insert' name fn
      | Def _ | Local _ | Err -> assert false

    method lookup_assoc_fn res name =
      match res with
      | Def (id, Struct) -> (assoc_fn#unsafe_get id)#get name
      | Ty _ -> assert false
      | Def _ | Local _ | Err -> assert false

    method lookup_method ty name =
      match self#lookup_method_def_id ty name with
      | Some did -> self#get_def did
      | None -> self#types.err

    method lookup_method_def_id ty name =
      match !ty with
      | Adt (did, _) ->
          assoc_fn#get did
          |> Option.map (fun map -> map#get name)
          |> Option.join
      | Err -> None
      | _ ->
          prim_ty_assoc_fn#get ty
          |> Option.map (fun map -> map#get name)
          |> Option.join

    method print_assoc_fns =
      let open Utils.Printer in
      let printer = new printer in
      assoc_fn#iter (fun did fns ->
          print_def_id did ^ "\n" |> green |> printer#append;
          render_children printer fns (fun (k, v) _ ->
              k |> cyan |> printer#append;
              printer#append " ";
              print_def_id v
              |> q
              |> green ?bold:(Some false)
              |> printer#append));
      printer#print

    method ast_ty_to_res (ty : Ast.ty) =
      match ty.kind with
      | Path path -> Some (res_map#unsafe_get path.path_id)
      | Err -> None
      | _ -> Some (Ty (self#ast_ty_to_ty ty))

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
    method autoderef ty = match !ty with Ptr ty | Ref ty -> ty | _ -> ty

    method fn_ptr args ret is_variadic abi =
      self#intern (FnPtr { args; ret; is_variadic; abi })

    method adt def_id subst = self#intern (Adt (def_id, subst))
    method get_adt def_id = adt_def#unsafe_get def_id
    method fn def_id subst = self#intern (Fn (def_id, subst))
    method get_fn def_id = fn_def#unsafe_get def_id

    method subst fnsig (Subst subst) =
      SubstFolder.fold_fnsig self fnsig subst

    method ty_with_subst ty =
      match !ty with
      | FnPtr { args; ret; is_variadic; abi } ->
          self#fn_ptr args ret is_variadic abi
      | Fn (def_id, subst) ->
          let fnsig = self#get_fn def_id in
          let { args; ret; is_variadic; abi } = self#subst fnsig subst in
          self#fn_ptr args ret is_variadic abi
      | _ -> assert false

    method adt_with_variants def_id variants =
      adt_def#insert' def_id { variants };
      self#adt def_id

    method fn_with_sig
        ?(subst = Subst (new vec))
        def_id
        args
        ret
        is_variadic
        abi =
      fn_def#insert' def_id { args; ret; is_variadic; abi };
      self#fn def_id subst

    method ty_param index name = self#intern (Param { index; name })
    method ty_param_from_def_id def_id = self#get_def def_id

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
      | Ptr _ | Ref _ | FnPtr _ | Fn _ -> 8
      | Str -> 16
      | Unit -> 0
      | Bool -> 1
      | _ -> assert false

    method non_enum_variant ty =
      match !ty with
      | Adt (def_id, Subst subst) ->
          let adt = SubstFolder.fold_adt self (self#get_adt def_id) subst in
          let variants = adt.variants in
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
          let args = (path.segments#last |> Option.get).args in
          let subst =
            match args with
            | Some args ->
                map args (fun arg : generic_arg ->
                    Ty (self#ast_ty_to_ty arg))
            | None -> new vec
          in
          let res = res_map#unsafe_get path.path_id in
          res |> ( function
          | Def (def_id, Struct) -> self#adt def_id (Subst subst)
          | Def (def_id, TyParam) -> self#ty_param_from_def_id def_id
          | _ -> assert false )
      | ImplicitSelf ->
          let res = res_map#unsafe_get ty.ty_id in
          res |> ( function
          | Def (def_id, _) ->
              (* TODO: maybe save the type parameters somewhere *)
              self#adt def_id (Subst (new vec))
          | Ty ty -> ty
          | _ -> assert false )
      | CVarArgs -> assert false

    method inner_ty ty : ty ref option =
      match !ty with
      | Ptr ty | Ref ty | FnPtr { ret = ty; _ } -> Some ty
      | _ -> None

    method render_ty_segments ty =
      let segments = new vec in
      (match !ty with
       | Ptr ty ->
           segments#push "p";
           segments#append (self#render_ty_segments ty)
       | Ref ty ->
           segments#push "r";
           segments#append (self#render_ty_segments ty)
       | Adt (def_id, _) ->
           self#def_key def_id |> ( function
           | { data = TypeNs name; _ } -> segments#push name
           | _ -> assert false )
       | Err -> assert false
       | _ -> segments#push (render_ty ty));
      segments

    method render_subst (subst : generic_arg vec) =
      if subst#empty
      then ""
      else
        sprintf
          "[%s]"
          (subst#join ", " (function Ty ty -> self#render_ty ty))

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
      | FnPtr fnsig -> Fn.render self fnsig
      | Ptr ty -> "*" ^ self#render_ty ty
      | Ref ty -> "&" ^ self#render_ty ty
      | Err -> "err"
      | Fn (def_id, Subst subst) ->
          let { args; ret; is_variadic; abi } = self#get_fn def_id in
          let args =
            map args (fun ty -> SubstFolder.fold_ty self ty subst)
          in
          let ret = SubstFolder.fold_ty self ret subst in
          sprintf
            "%sfn%s(%s) -> %s"
            (abi |> function
             | Default -> ""
             | Intrinsic -> "\"intrinsic\" "
             | C -> "\"C\" ")
            (self#render_subst subst)
            (args#join ", " (fun ty -> self#render_ty ty)
             ^ if is_variadic then ", ..." else String.empty)
            (self#render_ty ret)
      | Adt (def_id, Subst subst) ->
          (self#def_key def_id).data |> ( function
          | TypeNs name -> name ^ self#render_subst subst
          | _ -> assert false )
      | Param { name; _ } -> name
  end
