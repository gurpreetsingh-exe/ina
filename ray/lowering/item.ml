open Ast
open Ir
open Printf

let mangle path =
  "_Z"
  ^ String.concat ""
      (List.map
         (fun seg -> sprintf "%d%s" (String.length seg) seg)
         path.segments)

let rec lower_fn (fn : func) (ctx : Context.t) : Func.t =
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
  let linkage_name = mangle (Option.get func_path) in
  let fn_ty =
    Func.{ name; args; ret_ty; is_variadic; abi; is_extern; linkage_name }
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
  Context.block_append ctx entry;
  let ret = Expr.lower_block body ctx in
  let builder = Builder.create (Option.get ctx.block) in
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

let lower_ast (ctx : Context.t) : Module.t =
  let items = ref [] in
  let f (item : item) =
    match item with
    | Fn (func, _) -> items := !items @ [lower_fn func ctx]
    | Import _ -> ()
    | _ -> assert false
  in
  List.iter f ctx.modd.items;
  gen_id !items;
  { items = !items }
