open Printf
open Config

let usage arg0 =
  printf "Usage: %s [command] [options] input...\n" arg0;
  printf "\nCommands:\n";
  printf "    build            compile the file\n";
  printf "\nOptions:\n";
  printf "    -h, --help       print help information\n\n";
  exit 1

let value_map =
  let tbl = Hashtbl.create 0 in
  let entries =
    [|("--emit", ["asm"; "llvm-ir"; "exe"; "obj"]); ("--opt", ["0"; "3"])|]
  in
  Array.iter (fun (option, value) -> Hashtbl.add tbl option value) entries;
  tbl

let invalid_option_value option =
  fprintf stderr "unknown value for `%s`, expected one of %s\n\n" option
    (String.concat ", "
       (List.map (fun v -> sprintf "`%s`" v) (Hashtbl.find value_map option)));
  flush stderr;
  exit 1

let parse_args () =
  let argc = Array.length Sys.argv in
  let arg0 = Sys.argv.(0) in
  if argc < 2 then usage arg0;
  let command = ref Nan in
  let input_file = ref None in
  let output = ref Exe in
  let opt_level = ref Default in
  let i = ref (argc - 1) in
  while !i > 0 do
    let arg = Sys.argv.(argc - !i) in
    if String.starts_with ~prefix:"-" arg then
      if String.starts_with ~prefix:"--" arg then (
        match arg with
        | "--help" -> usage arg0
        | _ ->
            if String.contains arg '=' then (
              let pair = String.split_on_char '=' arg in
              let named_option, value = (List.hd pair, List.nth pair 1) in
              let f get_option_value value =
                match get_option_value value with
                | Some out -> out
                | None -> invalid_option_value named_option
              in
              match named_option with
              | "--emit" -> output := f get_output_type value
              | "--opt" -> opt_level := f get_opt_level value
              | _ ->
                  printf "Unknown option `%s`\n" named_option;
                  usage arg0)
            else (
              printf "Unknown option `%s`\n" arg;
              usage arg0))
      else usage arg0
    else if !command == Nan then (
      command :=
        match arg with
        | "build" -> Build
        | "test" -> Test
        | _ ->
            printf "unknown command `%s`\n" arg;
            usage arg0)
    else (
      match !command with Nan -> usage arg0 | _ -> input_file := Some arg);
    decr i
  done;
  match !input_file with
  | Some input ->
      let config = config input in
      config.opt_level <- !opt_level;
      config.output_type <- !output;
      config.command <- !command;
      config
  | None -> usage arg0
