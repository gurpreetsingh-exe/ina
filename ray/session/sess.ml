open Llvm_target
open Metadata

type timings = {
  mutable parse : float;
  mutable resolve : float;
  mutable sema : float;
  mutable lowering : float;
  mutable llvm : float;
  mutable gen_and_link : float;
}

let display timings =
  Printf.sprintf
    "    parsing: %f ms\n\
    \    resolve: %f ms\n\
    \    sema: %f ms\n\
    \    lowering: %f ms\n\
    \    llvm: %f ms\n\
    \    generation and linking: %f ms\n"
    timings.parse timings.resolve timings.sema timings.lowering timings.llvm
    timings.gen_and_link

type target = {
  lltarget : Target.t;
  triple : string;
  ptr_size : int;
  data_layout : DataLayout.t;
}

let target _ =
  let triple = Target.default_triple () in
  let lltarget = Target.by_triple triple in
  let machine =
    TargetMachine.create ~triple ?cpu:(Some "generic") ?features:None
      ?level:(Some CodeGenOptLevel.Default) ?reloc_mode:(Some RelocMode.PIC)
      ?code_model:(Some CodeModel.Default) lltarget
  in
  let data_layout = TargetMachine.data_layout machine in
  ( {
      lltarget;
      triple;
      ptr_size = DataLayout.pointer_size data_layout;
      data_layout;
    },
    machine )

type t = {
  options : Config.t;
  timings : timings;
  target : target;
  enc : Encoder.t;
  machine : TargetMachine.t;
}

let create options =
  let target, machine = target () in
  {
    options;
    timings =
      {
        parse = 0.;
        resolve = 0.;
        sema = 0.;
        lowering = 0.;
        llvm = 0.;
        gen_and_link = 0.;
      };
    target;
    enc = Encoder.create ();
    machine;
  }
