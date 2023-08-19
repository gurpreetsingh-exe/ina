open Ir
open Ty
open Inst

type t = {
  tcx : tcx;
  mutable block : basic_block;
}

let _vreg = ref 0

let vreg () =
  let v = !_vreg in
  incr _vreg; v

let reset () = _vreg := 0

let create tcx (bb : basic_block) : t = { tcx; block = bb }

let with_ctx f (ctx : Context.t) builder =
  let tmp = builder.block in
  builder.block <- Option.get ctx.block;
  f builder;
  builder.block <- tmp

let add_inst_with_ty ty kind builder : value =
  let i = vreg () in
  let inst = { kind; id = i } in
  builder.block.insts <- builder.block.insts @ [inst];
  VReg (inst, i, ty)

let add_inst (kind : inst_kind) (builder : t) : unit =
  let inst = { kind; id = -1 } in
  builder.block.insts <- builder.block.insts @ [inst]

let alloca (ty : ty) (builder : t) : value =
  add_inst_with_ty (Ptr ty) (Alloca ty) builder

let br (cond : value) (true_bb : value) (false_bb : value) (builder : t) =
  add_inst (Br (cond, true_bb, false_bb)) builder;
  let true_bb = Inst.extract_block true_bb
  and false_bb = Inst.extract_block false_bb in
  Basicblock.append_succ builder.block [true_bb; false_bb]

let jmp (bb : value) (builder : t) =
  add_inst (Jmp bb) builder;
  let bb = Inst.extract_block bb in
  Basicblock.append_succ builder.block [bb]

let phi (ty : ty) (args : (value * value) list) (builder : t) : value =
  add_inst_with_ty ty (Phi (ty, args)) builder

let store (src : value) (dst : value) (builder : t) : unit =
  add_inst (Store (src, dst)) builder

let load (ptr : value) (builder : t) : value =
  let ty = get_ty ptr in
  let ty = match ty with Ptr ty | RefTy ty -> ty | _ -> assert false in
  add_inst_with_ty ty (Load ptr) builder

let gep (ty : ty) (ptr : value) (index : int) (builder : t) : value =
  let fty =
    match ty with
    | Struct (_, tys) ->
        let _, ty = List.nth tys index in
        unwrap_ty builder.tcx ty
    | _ -> assert false
  in
  add_inst_with_ty (Ptr fty) (Gep (ty, ptr, index)) builder

let call (ty : ty) (fn : value) (args : value list) (builder : t) : value =
  let ty = unwrap_ty builder.tcx ty in
  let ret_ty =
    match ty with FnTy (_, ret_ty, _) -> ret_ty | _ -> assert false
  in
  add_inst_with_ty ret_ty (Call (ty, fn, args)) builder

let intrinsic (ty : ty) (name : string) (args : value list) (builder : t) :
    value =
  let ty =
    match unwrap_ty builder.tcx ty with
    | FnTy (_, ret_ty, _) -> ret_ty
    | _ -> assert false
  in
  add_inst_with_ty ty (Intrinsic (name, args)) builder

let ptrtoint (value : value) (ty : ty) (builder : t) : value =
  add_inst_with_ty ty (PtrToInt (value, ty)) builder

let inttoptr (value : value) (ty : ty) (builder : t) : value =
  add_inst_with_ty ty (IntToPtr (value, ty)) builder

let ret (ret : value) (builder : t) : unit = add_inst (Ret ret) builder

let ret_unit (builder : t) : unit = add_inst RetUnit builder

let trap msg builder = add_inst (Trap msg) builder

let nop _ =
  let inst = { kind = Nop; id = -1 } in
  VReg (inst, -1, Unit)

let const_int (ty : ty) (value : int) : value = Const (Int value, ty)

let const_float (ty : ty) (value : float) : value = Const (Float value, ty)

let const_string (ty : ty) (value : string) : value = Const (Str value, ty)

let const_bool (ty : ty) (value : bool) : value = Const (Bool value, ty)

let const_struct (ty : ty) (values : value list) : value =
  Const (Struct values, ty)
