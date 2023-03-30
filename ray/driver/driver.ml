open Front
open Printf
open Tokenizer

type command =
  | Build
  | Test
  | Nan

let compiler_command = ref Nan

let print_command (cmd : command) =
  printf "%s\n"
    (match cmd with Build -> "build" | Test -> "test" | Nan -> "none")

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

exception Invalid_token

let () =
  let argc = Array.length Sys.argv in
  let arg0 = if argc > 1 then Sys.argv.(0) else exit 1 in
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
  | Some name -> (
      let ic = open_in name in
      let s = really_input_string ic (in_channel_length ic) in
      ignore (ctx.file_source <- s);
      close_in ic;
      let tokenizer = Tokenizer.tokenize name s in
      let i = ref 0 in
      try
        while true do
          let tok = Tokenizer.next tokenizer in
          match tok with
          | Some { kind = Eof; _ } -> raise Exit
          | Some t ->
              display_token t s;
              i := !i + 1 (* printf "%d %d %d\n" st e (e - st) *)
          | None -> raise Invalid_token
        done
        (* with Exit -> printf "%d\n" !i) *)
      with Exit -> ())
  | None -> exit 1
