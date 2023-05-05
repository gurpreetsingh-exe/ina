open Front
open Tokenizer
open Parser
open Ast
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
      ("i8", Prim I8);
      ("i16", Prim I16);
      ("i32", Prim I32);
      ("i64", Prim I64);
      ("isize", Prim Isize);
      ("u8", Prim U8);
      ("u16", Prim U16);
      ("u32", Prim U32);
      ("u64", Prim U64);
      ("usize", Prim Usize);
      ("f32", Prim F32);
      ("f64", Prim F64);
      ("bool", Prim Bool);
    ]
  in
  List.length (List.filter (fun (case, exp) -> f case <> exp) cases) = 0
