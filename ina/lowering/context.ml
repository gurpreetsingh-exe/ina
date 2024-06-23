open Middle.Ctx
open Structures.Hashmap
open Structures.Vec
open Ir

class lcx tcx =
  let dummy_builder () =
    new Builder.builder
      tcx
      { locals = new vec; bbs = new vec }
      (Basicblock.create ())
  in
  object (self)
    val tcx : tcx = tcx
    val mutable fn : Func.t option = None
    val mdl : Module.t = { items = new vec }
    val mutable builder = dummy_builder ()
    val locals : Inst.value nodemap = new hashmap
    val break_bb : Inst.basic_block nodemap = new hashmap
    val loop_args : (Inst.value * Inst.value) vec nodemap = new hashmap
    method tcx = tcx
    method bx = builder
    method set_active_fn f = fn <- Some f
    method break_bb = break_bb
    method loop_args = loop_args

    method is_test_func =
      match (Option.get fn).instance.def with Test _ -> true | _ -> false

    method builder_at_end bb =
      builder <- new Builder.builder tcx (Option.get fn).basic_blocks bb

    method with_block bb f =
      let tmp = builder in
      builder <- new Builder.builder tcx (Option.get fn).basic_blocks bb;
      let () = f () in
      builder <- tmp

    method locals = locals
    method define fn = mdl.items#push fn
    method mdl = mdl

    method entry_block =
      match fn with
      | Some fn ->
          let bb = Basicblock.create () in
          bb.is_entry <- true;
          fn.basic_blocks.bbs#push bb;
          bb
      | None -> assert false

    method append_block bb =
      match fn with
      | Some fn -> fn.basic_blocks.bbs#push bb
      | None -> assert false

    method append_block_with_builder bb =
      self#append_block bb;
      self#builder_at_end bb

    method append_block' =
      match fn with
      | Some fn ->
          let bb = Basicblock.create () in
          fn.basic_blocks.bbs#push bb;
          bb
      | None -> assert false
  end
