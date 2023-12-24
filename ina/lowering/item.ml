open Ast
open Middle.Ty
open Ir
open Structures.Vec

let rec lower (lcx : Context.lcx) mdl =
  let tcx = lcx#tcx in
  let lower_fn fn =
    let ty =
      tcx#def_id_to_ty#unsafe_get { inner = fn.func_id; extmod_id = 0 }
    in
    let arg_tys =
      match !ty with FnPtr { args; _ } -> args | _ -> assert false
    in
    let def_id = tcx#node_id_to_def_id#unsafe_get fn.func_id in
    let args =
      mapi fn.fn_sig.args (fun i { arg; _ } ->
          Inst.Param (arg_tys#get i, arg, i))
    in
    let ifn =
      Func.
        {
          ty
        ; def_id
        ; args
        ; basic_blocks = { locals = new vec; bbs = new vec }
        ; decl = fn.is_extern
        }
    in
    lcx#set_active_fn ifn;
    (match fn.body with
     | Some body ->
         let bb = lcx#entry_block in
         lcx#builder_at_end bb;
         fn.fn_sig.args#iteri (fun i { arg_id; _ } ->
             args#get i |> function
             | Param (ty, _, _) as inst ->
                 let ptr = lcx#bx#alloca ty (Source.Span.make 0 0) in
                 assert (lcx#locals#insert arg_id ptr = None);
                 lcx#bx#store inst ptr (Source.Span.make 0 0)
             | _ -> assert false);
         let ret = Expr.lower_block lcx body in
         (match ret with
          | VReg inst ->
              (match inst.kind, inst.ty with
               | Nop, _ | _, { contents = Unit } -> lcx#bx#ret_unit
               | _ -> lcx#bx#ret ret)
          | Const _ | Global _ -> lcx#bx#ret ret
          | _ -> lcx#bx#ret_unit)
     | None -> ());
    Func.gen_id ifn.basic_blocks;
    lcx#define ifn
  in
  let f : Ast.item -> unit = function
    | Fn (fn, _) -> lower_fn fn
    | Mod { resolved_mod = Some mdl; _ } -> lower lcx mdl
    | Impl { impl_items; _ } ->
        impl_items#iter (function AssocFn fn -> lower_fn fn)
    | Type _ | ExternMod _ -> ()
    | Foreign fns -> fns#iter lower_fn
    | _ -> assert false
  in
  mdl.items#iter f
;;
