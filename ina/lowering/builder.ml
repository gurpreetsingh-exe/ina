open Ir
open Middle.Ctx
open Inst

class builder tcx blocks block =
  object (self)
    val tcx : tcx = tcx
    val blocks : Func.blocks = blocks
    val block : Inst.basic_block = block
    method block = block

    method private add_inst kind span =
      let inst = { kind; ty = tcx#types.unit; id = -1; span } in
      block.insts#push inst

    method private add_terminator term = block.terminator <- term

    method private add_inst_with_ty ty kind span =
      let inst = { kind; ty; id = -1; span } in
      block.insts#push inst;
      VReg inst

    method alloca ty span =
      let kind = Alloca ty in
      let ty = tcx#ptr ty in
      let inst = { kind; ty; id = blocks.locals#len; span } in
      blocks.locals#push inst;
      VReg inst

    method binary kind left right =
      let ty = get_ty tcx right in
      let inst = Binary (kind, left, right) in
      self#add_inst_with_ty ty inst

    method store src dst = self#add_inst (Store (src, dst))

    method copy ptr =
      let ty = get_ty tcx ptr in
      let ty = Option.get @@ tcx#inner_ty ty in
      self#add_inst_with_ty ty (Copy ptr)

    method move ptr =
      let ty = get_ty tcx ptr in
      let ty = Option.get @@ tcx#inner_ty ty in
      self#add_inst_with_ty ty (Move ptr)

    method gep ty ptr ident =
      let (Variant variant) = tcx#non_enum_variant ty in
      let index = ref (-1) in
      Structures.Vec.find
        (fun (Middle.Ty.Field { name; ty }) ->
          incr index;
          if name = ident then Some (!index, ty) else None)
        variant.fields
      |> function
      | Some (index, ty') ->
          self#add_inst_with_ty (tcx#ptr ty') (Gep (ty, ptr, index))
      | None -> assert false

    method call ty value args =
      let ret = Option.get @@ tcx#inner_ty ty in
      self#add_inst_with_ty ret (Call (ty, value, args))

    method br cond then_block else_block =
      self#add_terminator (Br (cond, then_block, else_block));
      let then_block = Inst.extract_block then_block
      and else_block = Inst.extract_block else_block in
      Basicblock.append_succ block then_block;
      Basicblock.append_succ block else_block

    method jmp bb =
      self#add_terminator (Jmp bb);
      let bb = Inst.extract_block bb in
      Basicblock.append_succ block bb

    method phi ty args = self#add_inst_with_ty ty (Phi (ty, args))

    method bitcast value ty =
      assert (tcx#sizeof (get_ty tcx value) = tcx#sizeof ty);
      let inst = BitCast (value, ty) in
      self#add_inst_with_ty ty inst

    method ptrtoint value ty =
      let inst = PtrToInt (value, ty) in
      self#add_inst_with_ty ty inst

    method inttoptr value ty =
      let inst = IntToPtr (value, ty) in
      self#add_inst_with_ty ty inst

    method zext value ty =
      let inst = Zext (value, ty) in
      self#add_inst_with_ty ty inst

    method trunc value ty =
      let inst = Trunc (value, ty) in
      self#add_inst_with_ty ty inst

    method nop =
      let open Source.Span in
      let inst =
        { kind = Nop; ty = tcx#types.unit; id = -1; span = make 0 0 }
      in
      VReg inst

    method ret ret = self#add_terminator (Ret ret)
    method ret_unit = self#add_terminator RetUnit
    method const_int ty value = Const { kind = Int value; ty }
    method const_float ty value = Const { kind = Float value; ty }
    method const_string ty value = Const { kind = Str value; ty }
    method const_bool ty value = Const { kind = Bool value; ty }
  end
