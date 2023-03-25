type literal =
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Bool of bool

type token_kind =
  | Ident
  | Lit of literal
  | Semi

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
  if tokenizer.id < String.length !(tokenizer.src) then (
    tokenizer.id <- tokenizer.id + 1;
    tokenizer.c <- Some !(tokenizer.src).[tokenizer.id])
  else raise End_of_file

let next tokenizer : token option =
  let tok = ref None in
  let start = (tokenizer.filename, tokenizer.id) in
  try
    while true do
      match tokenizer.c with
      | Some c -> (
        match c with
        | ' ' | '\n' | '\t' | '\r' -> bump tokenizer
        | 'a' .. 'z' | 'A' .. 'Z' -> (
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
        | ';' ->
            bump tokenizer;
            mk_tok tokenizer Semi tok start;
            raise Exit
        | _ ->
            Printf.printf "unknown char `%c`\n" c;
            exit 1)
      | None -> raise Exit
    done;
    !tok
  with Exit -> !tok

let tokenize filename source : tokenizer =
  let tokenizer = { c = None; id = -1; src = ref source; filename } in
  bump tokenizer; tokenizer
