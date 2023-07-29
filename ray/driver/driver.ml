open Printf
open Front
open Sema
open Codegen
open Resolve
open Session
open Utils
open Ty

let open_file (ctx : Sess.t) =
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
  let sess = Sess.create (Args.parse_args ()) in
  let tcx = tcx_create sess in
  let s = open_file sess in
  let tokenizer = Tokenizer.tokenize sess.options.input s in
  let pctx = Parser.parse_ctx_create tcx tokenizer s in
  let time, modd = Timer.time (fun () -> Parser.parse_mod pctx) in
  sess.timings.parse <- time;
  (match sess.options.command with
  | Build ->
      let time, resolutions =
        Timer.time (fun () ->
            let resolver = Resolver.create tcx modd in
            Resolver.resolve resolver)
      in
      printf "%s\n" (Resolver.print_resolutions resolutions 0);
      print_endline (print_def_table tcx.def_table);
      sess.timings.resolve <- time;
      let env = Hashtbl.create 0 in
      let time, _ =
        Timer.time (fun () ->
            let infer_ctx = Infer.infer_ctx_create pctx.emitter tcx env in
            ignore (Infer.infer_begin infer_ctx modd);
            let ty_ctx = Tychk.ty_ctx_create infer_ctx in
            ignore (Tychk.tychk ty_ctx modd))
      in
      sess.timings.sema <- time;
      if !Infer.error <> 0 || !Tychk.error <> 0 then exit 1;
      let time, modulee =
        Timer.time (fun () ->
            let lowering_ctx = Lowering.Context.create modd env in
            Lowering.Item.lower_ast lowering_ctx)
      in
      sess.timings.lowering <- time;
      if sess.options.print_ir then Ir.Module.render modulee;
      if sess.options.dot_cfg then Ir.Module.dot_graph modulee;
      let time, modd =
        Timer.time (fun () -> Llvm_gen.gen_module sess modulee)
      in
      sess.timings.llvm <- time;
      let time, _ = Timer.time (fun () -> Llvm_gen.emit modd sess) in
      sess.timings.gen_and_link <- time
  | Fmt -> printf "%s" (Fmt.render_mod modd)
  | _ -> ());
  let time = (Sys.time () -. start) *. 1000. in
  if sess.options.display_time then
    printf "  total: %f ms\n%s" time (Sess.display sess.timings);
  exit 0
