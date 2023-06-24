open Ir
open Inst

type t = { mutable block : basic_block }

let _vreg = ref 0

let vreg () =
  let v = !_vreg in
  incr _vreg; v

let create (bb : basic_block) : t = { block = bb }

let with_ctx f (ctx : Context.t) builder =
  let tmp = builder.block in
  builder.block <- Option.get ctx.block;
  f builder;
  builder.block <- tmp

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

let br (cond : value) (true_bb : value) (false_bb : value) (builder : t) =
  add_inst (Br (cond, true_bb, false_bb)) builder

let jmp (bb : value) (builder : t) = add_inst (Jmp bb) builder

let phi (ty : Ast.ty) (args : (value * value) list) (builder : t) : value =
  add_inst_with_ty ty (Phi (ty, args)) builder

let store (dst : Inst.value) (src : Inst.value) (builder : t) : unit =
  add_inst (Store (dst, src)) builder

let load (ptr : Inst.value) (builder : t) : value =
  let ty = get_ty ptr in
  let ty = match ty with Ptr ty -> ty | _ -> assert false in
  add_inst_with_ty ty (Load ptr) builder

let ret (ret : Inst.value) (builder : t) : unit = add_inst (Ret ret) builder

let ret_unit (builder : t) : unit = add_inst RetUnit builder

let nop _ =
  let inst = Inst.{ kind = Nop; id = -1 } in
  VReg (inst, -1, Unit)

let const_int (ty : Ast.ty) (value : int) : value = Const (Int value, ty)
