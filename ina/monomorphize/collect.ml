open Ir
open Module
open Func
open Inst
open Middle
open Ctx
open Ty
open Structures.Vec
open Structures.Hashmap

let collect tcx mdl =
  let mono_items = new vec in
  let generic_items = new hashmap in
  let instantiated_items = new vec in
  let rec instantiate ty =
    match !ty with
    | Fn (def_id, subst) ->
        let fn = generic_items#unsafe_get def_id in
        let fn = monomorphize fn subst in
        mono_items#push fn
    | _ -> assert false
  and monomorphize fn (Subst subst) =
    let fold_ty ty = SubstFolder.fold_ty tcx ty subst in
    let fold_instance instance = { instance with subst = Subst subst } in
    let rec fold_value value =
      match value with
      | Const const -> Const const
      | VReg inst -> VReg (fold_inst2 inst) (* this might be sus *)
      | Label bb -> Label bb
      | Param (ty, name, i) -> Param (fold_ty ty, name, i)
      | Global (Fn instance) -> Global (Fn (fold_instance instance))
    and fold_pair (value, ty) = fold_value value, fold_ty ty
    and fold_inst2 inst = { inst with ty = fold_ty inst.ty }
    and fold_inst inst =
      let ty = fold_ty inst.ty in
      let kind =
        match inst.kind with
        | Alloca ty -> Alloca (fold_ty ty)
        | Binary (kind, left, right) ->
            Binary (kind, fold_value left, fold_value right)
        | Phi (ty, branches) ->
            Phi
              ( fold_ty ty
              , map branches (fun (bb, value) -> bb, fold_value value) )
        | Store (src, dst) -> Store (fold_value src, fold_value dst)
        | Copy ptr -> Copy (fold_value ptr)
        | Move ptr -> Move (fold_value ptr)
        | Gep (ty, value, i) -> Gep (fold_ty ty, fold_value value, i)
        | Call (ty, value, args) ->
            let ty =
              if Fn.is_generic ty
              then (
                let subst' = Fn.subst ty in
                let subst' = SubstFolder.fold_subst tcx subst' subst in
                let ty = Fn.with_subst tcx ty subst' in
                instantiate ty;
                ty)
              else ty
            in
            Call (ty, fold_value value, map args fold_value)
        | Intrinsic (name, args) -> Intrinsic (name, map args fold_value)
        | Trap msg -> Trap msg
        | BitCast pair -> BitCast (fold_pair pair)
        | Zext pair -> Zext (fold_pair pair)
        | Trunc pair -> Trunc (fold_pair pair)
        | PtrToInt pair -> PtrToInt (fold_pair pair)
        | IntToPtr pair -> IntToPtr (fold_pair pair)
        | Nop -> Nop
      in
      { inst with kind; ty }
    and fold_terminator = function
      | Br (cond, then', else') ->
          Br (fold_value cond, fold_value then', fold_value else')
      | Jmp value -> Jmp (fold_value value)
      | Ret value -> Ret (fold_value value)
      | RetUnit -> RetUnit
    in
    let fold_bb bb =
      {
        bb with
        insts = map bb.insts fold_inst
      ; terminator = fold_terminator bb.terminator
      }
    in
    let basic_blocks = fn.basic_blocks in
    let basic_blocks =
      {
        bbs = map basic_blocks.bbs fold_bb
      ; locals = map basic_blocks.locals fold_inst
      }
    in
    {
      fn with
      instance = fold_instance fn.instance
    ; ty = Fn.with_subst tcx fn.ty subst
    ; args = map fn.args fold_value
    ; basic_blocks
    }
  in
  mdl.items#iter (fun fn ->
      match Fn.is_generic fn.ty with
      | true ->
          assert (
            generic_items#insert (Inst.instance_def_id fn.instance) fn = None)
      | false ->
          fn.basic_blocks.bbs#iter (fun bb ->
              bb.insts#iter (fun inst ->
                  match inst.kind with
                  | Call (ty, _, _) when Fn.is_generic ty ->
                      instantiated_items#push ty
                  | _ -> ()));
          mono_items#push fn);
  instantiated_items#iter instantiate;
  mono_items
;;
