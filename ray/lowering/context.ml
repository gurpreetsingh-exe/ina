open Ir
open Ast

type env = {
  parent : env option;
  locals : (ident, Inst.value) Hashtbl.t;
}

type t = {
  func_map : (path, func) Hashtbl.t;
  mutable env : env;
  globl_env : (path, lang_item) Hashtbl.t;
  modd : modd;
  mutable fn : Func.t option;
  mutable block : Basicblock.t option;
}

let create modd env =
  {
    func_map = Hashtbl.create 0;
    env = { parent = None; locals = Hashtbl.create 0 };
    globl_env = env;
    modd;
    fn = None;
    block = None;
  }

let add_local ctx name ptr = Hashtbl.add ctx.env.locals name ptr

let find_local env name = Hashtbl.find env.locals name

let block_append ctx bb =
  ctx.block <- Some bb;
  let fn = Option.get ctx.fn in
  match fn with
  | Def { basic_blocks; _ } -> basic_blocks.bbs <- basic_blocks.bbs @ [bb]
  | _ -> ()
