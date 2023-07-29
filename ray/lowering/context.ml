open Ty
open Ir
open Ast

type env = {
  parent : env option;
  locals : (ident, Inst.value) Hashtbl.t;
}

type t = {
  tcx : tcx;
  func_map : (path, Func.fn_type) Hashtbl.t;
  mutable env : env;
  globl_env : (path, lang_item) Hashtbl.t;
  mutable modd : modd;
  mutable fn : Func.t option;
  mutable block : Inst.basic_block option;
}

let create tcx modd env =
  {
    tcx;
    func_map = Hashtbl.create 0;
    env = { parent = None; locals = Hashtbl.create 0 };
    globl_env = env;
    modd;
    fn = None;
    block = None;
  }

let add_local ctx name ptr = Hashtbl.add ctx.env.locals name ptr

let rec find_local env name =
  if Hashtbl.mem env.locals name then Hashtbl.find env.locals name
  else (
    match env.parent with
    | Some env -> find_local env name
    | None -> raise Not_found)

let block_append ctx bb =
  ctx.block <- Some bb;
  let fn = Option.get ctx.fn in
  match fn with
  | Def { basic_blocks; _ } -> basic_blocks.bbs <- basic_blocks.bbs @ [bb]
  | _ -> ()
