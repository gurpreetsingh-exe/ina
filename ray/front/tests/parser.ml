open Front
open Tokenizer
open Parser
open Ty
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

(* let%test "function with variadic arg" = test_mod "fn a(arg0: i32, ...)
   {}\n" *)

let%test "types" =
  let f input =
    let tokenizer = tokenize "<test>" input in
    let pctx = parse_ctx_create tokenizer input in
    let ty = parse_ty pctx in
    ty
  in
  let cases =
    [
      ("i8", Int I8);
      ("i16", Int I16);
      ("i32", Int I32);
      ("i64", Int I64);
      ("isize", Int Isize);
      ("u8", Int U8);
      ("u16", Int U16);
      ("u32", Int U32);
      ("u64", Int U64);
      ("usize", Int Usize);
      ("f32", Float F32);
      ("f64", Float F64);
      ("bool", Bool);
    ]
  in
  List.length (List.filter (fun (case, exp) -> f case <> exp) cases) = 0
