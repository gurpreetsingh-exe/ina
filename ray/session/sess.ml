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

type t = {
  options : Config.t;
  timings : timings;
}

let create options =
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
  }