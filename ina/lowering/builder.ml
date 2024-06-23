open Ir
open Middle.Ctx
open Middle.Ty
open Inst

class builder tcx blocks block =
  object (self)
    val tcx : tcx = tcx
    val blocks : Func.blocks = blocks
    val block : Inst.basic_block = block
    method block = block
    method blocks = blocks

    method private add_inst kind span =
      let inst = { kind; ty = tcx#types.unit; id = -1; span } in
      block.insts#push inst

    method private add_terminator term =
      match block.terminator with
      | RetUnit -> block.terminator <- term
      | _ -> ()

    method private add_inst_with_ty ty kind span =
      let inst = { kind; ty; id = -1; span } in
      block.insts#push inst;
      VReg inst

    method alloca ty span =
      let kind = Alloca ty in
      let ty = tcx#ptr Mut ty in
      let inst = { kind; ty; id = blocks.locals#len; span } in
      blocks.locals#push inst;
      VReg inst

    method binary kind left right =
      let ty = get_ty tcx right in
      let ty =
        match kind with
        | Lt | LtEq | Gt | GtEq | Eq | NotEq -> tcx#types.bool
        | _ -> ty
      in
      let inst = Binary (kind, left, right) in
      self#add_inst_with_ty ty inst

    method store src dst = self#add_inst (Store (src, dst))

    method switch ?default cond args =
      self#add_terminator (Switch (cond, args, default));
      args#iter (function bb, _ ->
          let bb = Inst.extract_block bb in
          Basicblock.append_succ block bb)

    method discriminant value =
      let ty = get_ty tcx value in
      assert (Option.is_some (tcx#variants ty));
      self#add_inst_with_ty tcx#types.u8 (Discriminant value)

    method payload value idx =
      let ty = get_ty tcx value in
      let ty = tcx#tuple_of_variant ty idx in
      self#add_inst_with_ty (tcx#ptr Mut ty) (Payload (value, idx))

    method aggregate adt args =
      match adt with
      | Adt (did, _, Subst subst) ->
          let ty = SubstFolder.fold_ty tcx (tcx#get_def did) subst in
          self#add_inst_with_ty ty (Aggregate (adt, args))
      | Slice ty | Array ty ->
          self#add_inst_with_ty ty (Aggregate (adt, args))

    method coercion kind value ty =
      self#add_inst_with_ty ty (Coercion (kind, value, ty))

    method copy ptr =
      let ty = get_ty tcx ptr in
      let ty = Option.get @@ tcx#inner_ty ty in
      self#add_inst_with_ty ty (Copy ptr)

    method move ptr =
      let ty = get_ty tcx ptr in
      let ty = Option.get @@ tcx#inner_ty ty in
      self#add_inst_with_ty ty (Move ptr)

    method gep ty ty' ptr index =
      self#add_inst_with_ty (tcx#ptr Mut ty') (Gep (ty, ptr, index))

    method index sty slice index =
      let ty = tcx#slice_inner sty in
      self#add_inst_with_ty ty (Index (sty, slice, index))

    method length value = self#add_inst_with_ty tcx#types.usize (Len value)

    method call ty value args =
      let ret = Fn.ret tcx ty in
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

    method trap value = self#add_inst (Trap value)

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
