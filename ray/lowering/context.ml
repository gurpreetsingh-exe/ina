open Ir
open Ast

type env = {
  parent : env option;
  locals : (ident, Inst.value) Hashtbl.t;
}

type t = {
  func_map : (path, func) Hashtbl.t;
  mutable env : env;
  modd : modd;
}

let create modd =
  {
    func_map = Hashtbl.create 0;
    env = { parent = None; locals = Hashtbl.create 0 };
    modd;
  }

let add_local ctx name ptr = Hashtbl.add ctx.env.locals name ptr

let find_local env name = Hashtbl.find env.locals name
