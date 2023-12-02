open Middle.Ctx
open Structures.Hashmap
open Structures.Vec
open Ir

class lcx tcx =
  let dummy_builder () = new Builder.builder tcx (Basicblock.create ()) in
  object
    val tcx : tcx = tcx
    val mutable fn : Func.t option = None
    val mdl : Module.t = { items = new vec }
    val mutable builder = dummy_builder ()
    val locals : Inst.value nodemap = new hashmap
    method tcx = tcx
    method builder = builder
    method set_active_fn f = fn <- Some f
    method builder_at_end bb = builder <- new Builder.builder tcx bb
    method locals = locals
    method define fn = mdl.items#push fn
    method mdl = mdl

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
