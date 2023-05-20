open Front
open Printf
open Sema
open Codegen

type command =
  | Build
  | Test
  | Nan

let compiler_command = ref Nan

type context = {
  mutable file_name : string option;
  mutable file_source : string;
}

let ctx = { file_name = None; file_source = "" }

let usage arg0 =
  printf "Usage: %s [command] [options] input...\n" arg0;
  printf "\nCommands:\n";
  printf "    build            compile the file\n";
  printf "\nOptions:\n";
  printf "    -h, --help       print help information\n\n";
  exit 1

let () =
  let argc = Array.length Sys.argv in
  let arg0 = Sys.argv.(0) in
  if argc < 2 then usage arg0;
  let i = ref (argc - 1) in
  while !i > 0 do
    let arg = Sys.argv.(argc - !i) in
    if String.starts_with ~prefix:"-" arg then
      if String.starts_with ~prefix:"--" arg then (
        match arg with
        | "--help" -> usage arg0
        | _ ->
            printf "Unknown option `%s`\n" arg;
            usage arg0)
      else usage arg0
    else if !compiler_command == Nan then (
      compiler_command :=
        match arg with
        | "build" -> Build
        | "test" -> Test
        | _ ->
            printf "Unknown command `%s`\n" arg;
            usage arg0)
    else (
      match !compiler_command with
      | Nan -> usage arg0
      | _ -> ctx.file_name <- Some arg);
    decr i
  done;
  match ctx.file_name with
  | Some name ->
      let ic = open_in name in
      let s = really_input_string ic (in_channel_length ic) in
      ignore (ctx.file_source <- s);
      close_in ic;
      let tokenizer = Tokenizer.tokenize name s in
      let pctx = Parser.parse_ctx_create tokenizer s in
      let modd = Parser.parse_mod pctx in
      let infer_ctx = Infer.infer_ctx_create () in
      ignore (Infer.infer_begin infer_ctx modd);
      let ty_ctx = Tychk.ty_ctx_create infer_ctx in
      ignore (Tychk.tychk ty_ctx modd);
      if !Infer.error <> 0 then exit 1;
      (* print_endline (Fmt.display_mod modd); *)
      let modd = Llvm_gen.gen_module name modd in
      let out = String.split_on_char '.' name in
      Llvm_gen.emit modd (List.hd out)
  | None -> usage arg0
