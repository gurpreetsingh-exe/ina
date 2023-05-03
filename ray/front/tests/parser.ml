open Front
open Tokenizer
open Parser
open Fmt

let render_input f g input =
  let tokenizer = tokenize "<test>" input in
  let pctx = parse_ctx_create tokenizer input in
  let modd = f pctx in
  g modd = input

let test_mod input = render_input parse_mod render_mod input

let%test "function" =
  let cases = ["fn main() {}\n"; "extern fn other() {}\n"] in
  List.length (List.filter (fun case -> not (test_mod case)) cases) = 0

let%test "function with args" =
  test_mod "fn a(arg0: i32) {}\n"
  && test_mod "fn a(arg0: i32, arg1: i64) {}\n"

let%test "function with variadic arg" = test_mod "fn a(arg0: i32, ...) {}\n"
