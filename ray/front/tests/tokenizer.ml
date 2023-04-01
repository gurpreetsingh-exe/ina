open Printf
open Front
open Tokenizer
open Token

exception Invalid_token

let helper s =
  let tokenizer = tokenize "<test>" s in
  (tokenizer, next tokenizer)

let single_token s =
  let tokenizer, t = helper s in
  (match t with
  | Some { kind = Eof; _ } -> raise Exit
  | Some t ->
      let { start = _, st; ending = _, e } = t.span in
      printf "%s\n" (String.sub s st (e - st))
  | None -> raise Invalid_token);
  let t = next tokenizer in
  match t with
  | Some { kind = Eof; _ } -> ()
  | Some _ | None -> raise Invalid_token

let do_test s sep =
  let tokenizer = tokenize "<test>" s in
  try
    while true do
      let t = next tokenizer in
      match t with
      | Some { kind = Eof; _ } -> raise Exit
      | Some t ->
          let { start = _, st; ending = _, e } = t.span in
          printf sep (String.sub s st (e - st))
      | None -> raise Invalid_token
    done
  with Exit -> ()

let do_test_multiline s = do_test s "%s\n"

let do_test_singleline s = do_test s "%s"

let%test "keywords" =
  let _, t = helper "fn" in
  (Option.get t).kind = Fn

let%expect_test "single tokens" =
  single_token "20";
  single_token "20.20";
  single_token "a";
  single_token "\"string\"";
  single_token ";";
  [%expect {|
    20
    20.20
    a
    "string"
    ;
  |}];
  single_token "/";
  [%expect {|/|}];
  single_token "//";
  [%expect {|//|}];
  single_token "///";
  [%expect {|///|}];
  single_token "//!";
  [%expect {|//!|}]

let%expect_test "tokens" =
  do_test "fn main()" "%s\n";
  [%expect {|
    fn
    main
    (
    )
  |}];
  do_test "(:)[=]{,}" "%s";
  [%expect {|(:)[=]{,}|}]
