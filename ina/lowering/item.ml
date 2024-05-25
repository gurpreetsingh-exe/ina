open Ast
open Middle.Ty
open Middle.Def_id
open Ir
open Structures.Vec

let rec lower (lcx : Context.lcx) mdl =
  let tcx = lcx#tcx in
  let lower_fn fn =
    let def_id = local_def_id fn.func_id in
    let ty = tcx#get_def def_id in
    let subst = Middle.Ty.Fn.subst ty in
    let arg_tys = Fn.args tcx ty in
    let def : Inst.instance_def =
      if Fn.abi tcx ty = Intrinsic then Intrinsic def_id else Fn def_id
    in
    let instance = Inst.{ def; subst = Subst subst } in
    let args =
      mapi fn.fn_sig.args (fun i { arg; _ } ->
          Inst.Param (arg_tys#get i, arg, i))
    in
    let ifn =
      Func.
        {
          ty
        ; instance
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
    | Fn (fn, attrs) when is_test attrs && tcx#sess.options.command = Test ->
        let def_id = local_def_id fn.func_id in
        let ty = tcx#get_def def_id in
        let instance = Inst.{ def = Test def_id; subst = Subst (new vec) } in
        let ifn =
          Func.
            {
              ty
            ; instance
            ; args = new vec
            ; basic_blocks = { locals = new vec; bbs = new vec }
            ; decl = false
            }
        in
        lcx#set_active_fn ifn;
        let bb = lcx#entry_block in
        lcx#builder_at_end bb;
        let _ = Expr.lower_block lcx (fn.body |> Option.get) in
        lcx#define ifn
    | Fn (_, attrs) when is_test attrs -> ()
    | Fn (fn, _) -> lower_fn fn
    | Mod { resolved_mod = Some mdl; _ } -> lower lcx mdl
    | Impl { items; _ } -> items#iter (function AssocFn fn -> lower_fn fn)
    | Type _ | ExternMod _ | Using _ -> ()
    | Foreign (fns, _) -> fns#iter lower_fn
    | _ -> assert false
  in
  mdl.items#iter f
;;
