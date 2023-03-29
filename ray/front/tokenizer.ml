type literal =
  | Int of int
  | Float of float
  | Char of char
  | Bool of bool
  | String

type token_kind =
  | Ident
  | Lit of literal
  | Semi
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Colon
  | Eq
  | Comma
  | Slash
  | Eof

type pos = string * int

type span = {
  start : pos;
  ending : pos;
}

type token = {
  kind : token_kind;
  span : span;
}

type tokenizer = {
  mutable c : char option;
  mutable id : int;
  src : string ref;
  filename : string;
}

let mk_tok (tokenizer : tokenizer) (kind : token_kind)
    (tok : token option ref) (start : pos) =
  tok :=
    Some
      { kind; span = { start; ending = (tokenizer.filename, tokenizer.id) } }

let bump tokenizer =
  if tokenizer.id < String.length !(tokenizer.src) - 1 then (
    tokenizer.id <- tokenizer.id + 1;
    tokenizer.c <- Some !(tokenizer.src).[tokenizer.id])
  else (
    tokenizer.c <- None;
    tokenizer.id <- tokenizer.id + 1)

let get_token_type c : token_kind =
  match c with
  | ';' -> Semi
  | '(' -> LParen
  | ')' -> RParen
  | '[' -> LBracket
  | ']' -> RBracket
  | '{' -> LBrace
  | '}' -> RBrace
  | ':' -> Colon
  | '=' -> Eq
  | ',' -> Comma
  | '/' -> Slash
  | _ -> raise Exit

let next tokenizer : token option =
  let tok = ref None in
  try
    while true do
      match tokenizer.c with
      | Some (' ' | '\n' | '\t' | '\r') -> bump tokenizer
      | Some '0' .. '9' -> (
          let start = (tokenizer.filename, tokenizer.id) in
          let exception I in
          let is_float = ref false in
          let buf : string ref = ref "" in
          try
            while true do
              match tokenizer.c with
              | Some (('0' .. '9' | '.') as c) ->
                  buf := !buf ^ String.make 1 c;
                  bump tokenizer;
                  if (not !is_float) && c == '.' then is_float := true
              | Some _ | None -> raise I
            done
          with I ->
            if !is_float then (
              let value = float_of_string !buf in
              mk_tok tokenizer (Lit (Float value)) tok start)
            else (
              let value = int_of_string !buf in
              mk_tok tokenizer (Lit (Int value)) tok start);
            raise Exit)
      | Some ('a' .. 'z' | 'A' .. 'Z' | '_') -> (
          let start = (tokenizer.filename, tokenizer.id) in
          let exception I in
          try
            while true do
              match tokenizer.c with
              | Some ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') ->
                  bump tokenizer
              | Some _ | None -> raise I
            done
          with I ->
            mk_tok tokenizer Ident tok start;
            raise Exit)
      | Some '"' -> (
          let start = (tokenizer.filename, tokenizer.id) in
          bump tokenizer;
          let exception I in
          try
            while true do
              match tokenizer.c with
              | Some '"' -> bump tokenizer; raise I
              | Some _ -> bump tokenizer
              | None -> raise I
            done
          with I ->
            mk_tok tokenizer (Lit String) tok start;
            raise Exit)
      | Some c ->
          let start = (tokenizer.filename, tokenizer.id) in
          bump tokenizer;
          mk_tok tokenizer (get_token_type c) tok start;
          raise Exit
      | None ->
          let start = (tokenizer.filename, tokenizer.id) in
          mk_tok tokenizer Eof tok start;
          raise Exit
    done;
    !tok
  with Exit -> !tok

let tokenize filename source : tokenizer =
  let tokenizer = { c = None; id = -1; src = ref source; filename } in
  bump tokenizer; tokenizer
