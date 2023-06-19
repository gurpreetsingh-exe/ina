open Ir
open Inst

type t = { block : Basicblock.t }

let _vreg = ref 0

let vreg () =
  let v = !_vreg in
  incr _vreg; v

let create (bb : Basicblock.t) : t = { block = bb }

let add_inst_with_ty ty kind builder : value =
  let i = vreg () in
  let inst = Inst.{ kind; id = i } in
  builder.block.insts <- builder.block.insts @ [inst];
  VReg (inst, i, ty)

let add_inst (kind : inst_kind) (builder : t) : unit =
  let inst = Inst.{ kind; id = -1 } in
  builder.block.insts <- builder.block.insts @ [inst]

let alloca (ty : Ast.ty) (builder : t) : value =
  add_inst_with_ty (Ptr ty) (Alloca ty) builder

let store (dst : Inst.value) (src : Inst.value) (builder : t) : unit =
  add_inst (Store (dst, src)) builder

let load (ptr : Inst.value) (builder : t) : value =
  let ty = match ptr with VReg (_, _, ty) | Const (_, ty) -> ty in
  let ty = match ty with Ptr ty -> ty | _ -> assert false in
  add_inst_with_ty ty (Load ptr) builder

let ret (ret : Inst.value) (builder : t) : unit = add_inst (Ret ret) builder

let ret_unit (builder : t) : unit = add_inst RetUnit builder

let const_int (ty : Ast.ty) (value : int) : value = Const (Int value, ty)
