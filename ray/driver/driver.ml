open Printf
open Front
open Sema
open Codegen
open Resolve
open Session

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
  let context = Context.{ options = Args.parse_args () } in
  let s = open_file context in
  let tokenizer = Tokenizer.tokenize context.options.input s in
  let pctx = Parser.parse_ctx_create tokenizer s in
  let modd = Parser.parse_mod pctx in
  (match context.options.command with
  | Build ->
      let resolver = Imports.resolver_create modd in
      let env = Imports.resolve resolver in
      let infer_ctx = Infer.infer_ctx_create env in
      ignore (Infer.infer_begin infer_ctx modd);
      let ty_ctx = Tychk.ty_ctx_create infer_ctx in
      ignore (Tychk.tychk ty_ctx modd);
      if !Infer.error <> 0 then exit 1;
      let lowering_ctx = Lowering.Context.create modd env in
      let modulee = Lowering.Item.lower_ast lowering_ctx in
      if context.options.print_ir then Ir.Module.render modulee;
      let modd = Llvm_gen.gen_module context modulee in
      Llvm_gen.emit modd context
  | Fmt -> printf "%s" (Fmt.render_mod modd)
  | _ -> ());
  let time = (Sys.time () -. start) *. 1000. in
  if context.options.display_time then printf "  time: %f ms\n" time;
  exit 0
