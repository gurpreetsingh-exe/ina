open Printf
open Front
open Sema
open Codegen
open Resolve
open Session
open Utils

let open_file (ctx : Context.t) =
  let name = ctx.options.input in
  if Sys.file_exists name then (
    let ic = open_in name in
    let s = really_input_string ic (in_channel_length ic) in
    if not (String.is_valid_utf_8 s) then (
      fprintf stderr
        "error: cannot process `%s`, file is not valid UTF-8 string\n" name;
      exit 1);
    if Filename.extension name = "" then (
      fprintf stderr
        "error: input file `%s` will be overwritten by the executable\n" name;
      exit 1);
    ctx.options.output <- Filename.chop_extension name;
    close_in ic;
    s)
  else (
    fprintf stderr "error: cannot open `%s`, file doesn't exist\n" name;
    exit 1)

let () =
  let start = Sys.time () in
  let context = Context.create (Args.parse_args ()) in
  let s = open_file context in
  let tokenizer = Tokenizer.tokenize context.options.input s in
  let pctx = Parser.parse_ctx_create context tokenizer s in
  let time, modd = Timer.time (fun () -> Parser.parse_mod pctx) in
  context.timings.parse <- time;
  (match context.options.command with
  | Build ->
      let time, env =
        Timer.time (fun () ->
            let resolver = Imports.resolver_create context modd in
            Imports.resolve resolver)
      in
      context.timings.resolve <- time;
      let time, _ =
        Timer.time (fun () ->
            let infer_ctx =
              Infer.infer_ctx_create pctx.emitter context env
            in
            ignore (Infer.infer_begin infer_ctx modd);
            let ty_ctx = Tychk.ty_ctx_create infer_ctx in
            ignore (Tychk.tychk ty_ctx modd))
      in
      context.timings.sema <- time;
      if !Infer.error <> 0 || !Tychk.error <> 0 then exit 1;
      let time, modulee =
        Timer.time (fun () ->
            let lowering_ctx = Lowering.Context.create modd env in
            Lowering.Item.lower_ast lowering_ctx)
      in
      context.timings.lowering <- time;
      if context.options.print_ir then Ir.Module.render modulee;
      if context.options.dot_cfg then Ir.Module.dot_graph modulee;
      let time, modd =
        Timer.time (fun () -> Llvm_gen.gen_module context modulee)
      in
      context.timings.llvm <- time;
      let time, _ = Timer.time (fun () -> Llvm_gen.emit modd context) in
      context.timings.gen_and_link <- time
  | Fmt -> printf "%s" (Fmt.render_mod modd)
  | _ -> ());
  let time = (Sys.time () -. start) *. 1000. in
  if context.options.display_time then
    printf "  total: %f ms\n%s" time (Context.display context.timings);
  exit 0
