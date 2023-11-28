open Ir
open Middle.Ty
open Middle.Ctx
open Inst

class builder tcx block =
  object (self)
    val tcx : tcx = tcx
    val block : Inst.basic_block = block

    method private add_inst kind =
      let inst = { kind; ty = tcx#types.unit; id = -1 } in
      block.insts#push inst

    method private add_inst_with_ty ty kind =
      let inst = { kind; ty; id = -1 } in
      block.insts#push inst;
      VReg inst

    method alloca ty =
      let inst = Alloca ty in
      let ty = tcx#intern (Ptr !ty) in
      self#add_inst_with_ty ty inst

    method store src dst = self#add_inst (Store (src, dst))

    method load ptr =
      let ty = get_ty ptr in
      let ty = Option.get @@ tcx#inner_ty ty in
      self#add_inst_with_ty ty (Load ptr)

    method nop =
      let inst = { kind = Nop; ty = tcx#types.unit; id = -1 } in
      VReg inst

    method ret ret = self#add_inst (Ret ret)
    method ret_unit = self#add_inst RetUnit
    method const_int ty value = Const { kind = Int value; ty }
    method const_float ty value = Const { kind = Float value; ty }
    method const_string ty value = Const { kind = Str value; ty }
    method const_bool ty value = Const { kind = Bool value; ty }
  end
