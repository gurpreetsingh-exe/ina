open Middle.Ctx
open Structures.Hashmap
open Ir
open Ast

class lcx tcx mdl =
  let dummy_builder () = new Builder.builder tcx (Basicblock.create ()) in
  object
    val tcx : tcx = tcx
    val mdl : modd = mdl
    val mutable fn : Func.t option = None
    val mutable builder = dummy_builder ()
    val locals : Inst.value nodemap = new hashmap
    method tcx = tcx
    method mdl = mdl
    method builder = builder
    method set_active_fn f = fn <- Some f
    method builder_at_end bb = builder <- new Builder.builder tcx bb
    method locals = locals

    method entry_block =
      let open Func in
      match fn with
      | Some fn ->
          let bb = Basicblock.create () in
          bb.is_entry <- true;
          fn.basic_blocks.bbs#push bb;
          bb
      | None -> assert false

    method append_block =
      let open Func in
      match fn with
      | Some fn ->
          let bb = Basicblock.create () in
          fn.basic_blocks.bbs#push bb;
          bb
      | None -> assert false
  end
