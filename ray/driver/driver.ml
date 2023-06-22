open Front
open Sema
open Codegen
open Resolve
open Session

let open_file (ctx : Context.t) =
  let name = ctx.options.input in
  let ic = open_in name in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic; s

let () =
  let context = Context.{ options = Args.parse_args () } in
  let s = open_file context in
  let tokenizer = Tokenizer.tokenize context.options.input s in
  let pctx = Parser.parse_ctx_create tokenizer s in
  let modd = Parser.parse_mod pctx in
  match context.options.command with
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
      Ir.Module.render modulee;
      let modd = Llvm_gen.gen_module context modulee in
      Llvm_gen.emit modd context
  | Fmt -> Printf.printf "%s" (Fmt.render_mod modd)
  | _ -> ()
