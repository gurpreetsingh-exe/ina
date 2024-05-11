open Printf
open Config

let usage arg0 =
  printf "Usage: %s [command] [options] input...\n" arg0;
  printf "\nCommands:\n";
  printf "    build                Compile the file\n";
  printf "    check                Analyze the file and report errors\n";
  printf "    fmt                  Format the file\n";
  printf "\nOptions:\n";
  printf "        --emit=asm|exe|obj|extmod\n";
  printf "                         Output type to emit\n";
  printf "        --opt=0|3        Optimization level\n";
  printf "        --skip-codegen   Run every pass except codegen\n";
  printf "        --skip-passes[=PASS]\n";
  printf
    "                         Comma separated list of IR passes to skip;\n";
  printf "                         Specifying no value skips all passes.\n";
  printf "                         See `--list-passes` for more info.\n";
  printf
    "        --list-passes    List IR transformation/optimization passes\n";
  printf "        --print-ast      Print AST of the file\n";
  printf "        --print-ir       Print IR of the file\n";
  printf
    "        --dot-cfg        Emit cfg of function(s) to \".dot\" file(s)\n";
  printf "        --time           Print time info of all compiler passes\n";
  printf "        --ui-testing     Enable UI testing\n";
  printf "    -h, --help           Print help information\n\n";
  exit 1
;;

let value_map =
  let tbl = Hashtbl.create 0 in
  let entries =
    [|"--emit", ["asm"; "exe"; "obj"; "extmod"]; "--opt", ["0"; "3"]|]
  in
  Array.iter (fun (option, value) -> Hashtbl.add tbl option value) entries;
  tbl
;;

let passes : string array ref = ref [||]

let invalid_option_value option =
  fprintf
    stderr
    "unknown value for `%s`, expected one of %s\n\n"
    option
    (String.concat
       ", "
       (List.map (fun v -> sprintf "`%s`" v) (Hashtbl.find value_map option)));
  flush stderr;
  exit 1
;;

let get_skip_passes = function
  | "" -> Some [||]
  | v ->
      Some
        (String.split_on_char ',' v
         |> List.filter_map (function
                | "" -> None
                | name when Array.mem name !passes -> Some name
                | name ->
                    fprintf
                      stderr
                      "unknown value `%s` for `--skip-passes`\n"
                      name;
                    exit 1)
         |> Array.of_list)
;;

let parse_args () =
  let argc = Array.length Sys.argv in
  let arg0 = Sys.argv.(0) in
  if argc < 2 then usage arg0;
  let config = config () in
  let i = ref (argc - 1) in
  while !i > 0 do
    let arg = Sys.argv.(argc - !i) in
    (if String.starts_with ~prefix:"-" arg
     then
       if String.starts_with ~prefix:"--" arg
       then
         match arg with
         | "--help" -> usage arg0
         | "--print-ast" -> config.print_ast <- true
         | "--print-ir" -> config.print_ir <- true
         | "--print-module-graph" -> config.print_module_graph <- true
         | "--time" -> config.display_time <- true
         | "--type-vars" -> config.display_type_vars <- true
         | "--ui-testing" -> config.ui_testing <- true
         | "--dot-cfg" -> config.dot_cfg <- true
         | "--stdlib" -> config.build_stdlib <- true
         | "--skip-codegen" -> config.skip_codegen <- true
         | "--skip-passes" -> config.skip_passes <- Some [||]
         | "--list-passes" -> config.list_passes <- true
         | _ ->
             if String.contains arg '='
             then (
               let pair = String.split_on_char '=' arg in
               let named_option, value = List.hd pair, List.nth pair 1 in
               let f get_option_value value =
                 match get_option_value value with
                 | Some out -> out
                 | None -> invalid_option_value named_option
               in
               match named_option with
               | "--emit" -> config.output_type <- f get_output_type value
               | "--opt" -> config.opt_level <- f get_opt_level value
               | "--skip-passes" ->
                   config.skip_passes <- get_skip_passes value
               | _ ->
                   printf "Unknown option `%s`\n" named_option;
                   usage arg0)
             else (
               printf "Unknown option `%s`\n" arg;
               usage arg0)
       else usage arg0
     else if config.command == Nan
     then
       config.command <-
         (match arg with
          | "build" -> Build
          | "check" -> Check
          | "fmt" -> Fmt
          | "test" -> Test
          | _ ->
              printf "unknown command `%s`\n" arg;
              usage arg0)
     else
       match config.command with
       | Nan -> usage arg0
       | _ -> config.input <- arg);
    decr i
  done;
  config
;;
