open Ast
open Ty
open Ir

let mangle path =
  "_Z"
  ^ String.concat ""
      (List.map
         (fun seg -> string_of_int (String.length seg) ^ seg)
         path.segments)

(* let mangle path = render_path path *)

let rec lower_fn (fn : func) (ctx : Context.t) (mangle_name : bool) : Func.t
    =
  Builder.reset ();
  let {
    fn_sig = { name; args; ret_ty; is_variadic; _ };
    body;
    abi;
    is_extern;
    func_path;
    _;
  } =
    fn
  in
  let ret_ty = match ret_ty with Some ty -> ty | None -> Unit in
  let fn_path = Option.get func_path in
  let linkage_name =
    if mangle_name then lookup_sym ctx.tcx fn_path.res else name
  in
  let fn_ty =
    Func.
      {
        name;
        args =
          List.map (fun (ty, name, _) -> (unwrap_ty ctx.tcx ty, name)) args;
        params =
          List.mapi
            (fun i (ty, name, _) ->
              Inst.Param (unwrap_ty ctx.tcx ty, name, i))
            args;
        ret_ty;
        is_variadic;
        abi;
        is_extern;
        linkage_name;
      }
  in
  match body with
  | Some body ->
      let fn = Func.Def { def_ty = fn_ty; basic_blocks = { bbs = [] } } in
      ctx.fn <- Some fn;
      lower_fn_body body ctx;
      fn
  | None -> Decl fn_ty

and lower_fn_body body ctx =
  let entry = Basicblock.create () in
  entry.is_entry <- true;
  Context.block_append ctx entry;
  let ret = Expr.lower_block body ctx in
  let builder = Builder.create ctx.tcx (Option.get ctx.block) in
  match ret with
  | VReg (inst, _, _) -> (
    match inst.kind with
    | Nop -> Builder.ret_unit builder
    | _ -> Builder.ret ret builder)
  | Const _ -> Builder.ret ret builder
  | _ -> Builder.ret_unit builder

let gen_id (items : Func.t list) =
  let f (item : Func.t) =
    let bb_id = ref 0 in
    match item with
    | Def { basic_blocks; _ } ->
        List.iter
          (fun (bb : Inst.basic_block) ->
            bb.bid <- !bb_id;
            incr bb_id)
          basic_blocks.bbs
    | _ -> ()
  in
  List.iter f items

let rec lower_ast (ctx : Context.t) : Module.t =
  let items = ref [] in
  let f (item : item) =
    match item with
    | Fn (func, attrs) ->
        let mangle = ref true in
        List.iter
          (fun attr ->
            match attr.kind with
            | NormalAttr { name = "nomangle" } -> mangle := false
            | _ -> ())
          attrs;
        items := !items @ [lower_fn func ctx !mangle]
    | Foreign funcs ->
        items := !items @ List.map (fun f -> lower_fn f ctx false) funcs
    | Import _ -> ()
    | Mod m ->
        let modd = Option.get m.resolved_mod in
        let tmp = ctx.modd in
        ctx.modd <- modd;
        items := !items @ (lower_ast ctx).items;
        ctx.modd <- tmp
    | Const _ | Type _ | Unit _ -> ()
  in
  List.iter f ctx.modd.items;
  gen_id !items;
  { items = !items }
