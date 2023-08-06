open Printf
open Config

let usage arg0 =
  printf "Usage: %s [command] [options] input...\n" arg0;
  printf "\nCommands:\n";
  printf "    build                compile the file\n";
  printf "    fmt                  format the file\n";
  printf "\nOptions:\n";
  printf "        --emit=asm|llvm-ir|exe|obj|lib\n";
  printf "                         output type to emit\n";
  printf "        --opt=0|3        optimization level\n";
  printf "        --print-ir       print IR of the file\n";
  printf
    "        --dot-cfg        emit cfg of function(s) to \".dot\" file(s)\n";
  printf "        --time           print time info of all compiler passes\n";
  printf "        --ui-testing     enable ui testing\n";
  printf "    -h, --help           print help information\n\n";
  exit 1

let value_map =
  let tbl = Hashtbl.create 0 in
  let entries =
    [|
      ("--emit", ["asm"; "llvm-ir"; "exe"; "obj"; "lib"]);
      ("--opt", ["0"; "3"]);
    |]
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
  let config = config () in
  let i = ref (argc - 1) in
  while !i > 0 do
    let arg = Sys.argv.(argc - !i) in
    if String.starts_with ~prefix:"-" arg then
      if String.starts_with ~prefix:"--" arg then (
        match arg with
        | "--help" -> usage arg0
        | "--print-ir" -> config.print_ir <- true
        | "--time" -> config.display_time <- true
        | "--type-vars" -> config.display_type_vars <- true
        | "--ui-testing" -> config.ui_testing <- true
        | "--dot-cfg" -> config.dot_cfg <- true
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
              | "--emit" -> config.output_type <- f get_output_type value
              | "--opt" -> config.opt_level <- f get_opt_level value
              | _ ->
                  printf "Unknown option `%s`\n" named_option;
                  usage arg0)
            else (
              printf "Unknown option `%s`\n" arg;
              usage arg0))
      else usage arg0
    else if config.command == Nan then
      config.command <-
        (match arg with
        | "build" -> Build
        | "fmt" -> Fmt
        | "test" -> Test
        | _ ->
            printf "unknown command `%s`\n" arg;
            usage arg0)
    else (
      match config.command with
      | Nan -> usage arg0
      | _ -> config.input <- arg);
    decr i
  done;
  config
