open Token

type tokenizer = {
  mutable c : char option;
  mutable id : int;
  src : string ref;
  filename : string;
}

let keywords = Hashtbl.create 2;;

Hashtbl.add keywords "fn" Fn;
Hashtbl.add keywords "extern" Extern

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

let peek tokenizer =
  if tokenizer.id < String.length !(tokenizer.src) - 1 then
    Some !(tokenizer.src).[tokenizer.id + 1]
  else None

exception Invalid_token

let get_token_type c tokenizer : token_kind =
  match c with
  | ';' -> Semi
  | '(' -> LParen
  | ')' -> RParen
  | '[' -> LBracket
  | ']' -> RBracket
  | '{' -> LBrace
  | '}' -> RBrace
  | ':' -> Colon
  | '=' -> (
    match peek tokenizer with
    | Some '=' -> bump tokenizer; EqEq
    | Some _ | None -> Eq)
  | '!' -> (
    match peek tokenizer with
    | Some '=' -> bump tokenizer; BangEq
    | Some _ | None -> Bang)
  | ',' -> Comma
  | '/' -> (
    match peek tokenizer with
    | Some '/' ->
        bump tokenizer;
        Comment
          (match peek tokenizer with
          | Some '/' -> Some Outer
          | Some '!' -> Some Inner
          | Some _ | None -> None)
    | Some _ | None -> Slash)
  | '+' -> Plus
  | '-' -> (
    match peek tokenizer with
    | Some '>' -> bump tokenizer; Arrow
    | Some _ | None -> Minus)
  | '*' -> Star
  | '|' -> (
    match peek tokenizer with
    | Some '|' -> bump tokenizer; Pipe2
    | Some _ | None -> Pipe)
  | '&' -> (
    match peek tokenizer with
    | Some '&' -> bump tokenizer; Ampersand2
    | Some _ | None -> Ampersand)
  | '\000' -> Eof
  | _ -> raise Invalid_token

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
            mk_tok tokenizer
              (Lit (if !is_float then Float else Int))
              tok start;
            raise Exit)
      | Some ('a' .. 'z' | 'A' .. 'Z' | '_') -> (
          let start = (tokenizer.filename, tokenizer.id) in
          let buf = ref "" in
          let exception I in
          try
            while true do
              match tokenizer.c with
              | Some (('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') as c) ->
                  buf := !buf ^ String.make 1 c;
                  bump tokenizer
              | Some _ | None -> raise I
            done
          with I ->
            (match !buf with
            | "true" | "false" -> mk_tok tokenizer (Lit Bool) tok start
            | _ ->
                mk_tok tokenizer
                  (if Hashtbl.mem keywords !buf then
                   Hashtbl.find keywords !buf
                  else Ident)
                  tok start);
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
          let kind = get_token_type c tokenizer in
          (match kind with
          (* TODO: strip the `//*` from comments *)
          | Comment _ -> (
              let exception I in
              try
                while true do
                  match tokenizer.c with
                  | Some '\n' | None -> raise I
                  | Some _ -> bump tokenizer
                done
              with I -> ())
          | _ -> bump tokenizer);
          mk_tok tokenizer kind tok start;
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
