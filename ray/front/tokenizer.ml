open Token

type tokenizer = {
  mutable c : char option;
  src : string ref;
  mutable pos : pos;
  filename : string;
}

let keywords = Hashtbl.create 0;;

Hashtbl.add keywords "fn" Fn;
Hashtbl.add keywords "type" Type;
Hashtbl.add keywords "extern" Extern;
Hashtbl.add keywords "mod" Mod;
Hashtbl.add keywords "let" Let;
Hashtbl.add keywords "import" Import;
Hashtbl.add keywords "if" If;
Hashtbl.add keywords "else" Else;
Hashtbl.add keywords "assert" Assert;
Hashtbl.add keywords "as" As

let mk_tok (tokenizer : tokenizer) (kind : token_kind)
    (tok : token option ref) (start : pos) =
  tok := Some { kind; span = { start; ending = tokenizer.pos } }

let bump tokenizer =
  let filename, id, line, col = tokenizer.pos in
  if id < String.length !(tokenizer.src) - 1 then (
    let c = !(tokenizer.src).[id + 1] in
    tokenizer.pos <-
      (if c = '\n' then (filename, id + 1, line + 1, 0)
      else (filename, id + 1, line, col + 1));
    tokenizer.c <- Some c)
  else (
    tokenizer.c <- None;
    tokenizer.pos <- (filename, id + 1, line, col + 1))

let peek tokenizer =
  let _, id, _, _ = tokenizer.pos in
  if id < String.length !(tokenizer.src) - 1 then
    Some !(tokenizer.src).[id + 1]
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
  | ':' -> (
    match peek tokenizer with
    | Some ':' -> bump tokenizer; Colon2
    | Some _ | None -> Colon)
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
  | '.' -> (
    match peek tokenizer with
    | Some '.' -> (
        bump tokenizer;
        match peek tokenizer with
        | Some '.' -> bump tokenizer; Dot3
        | Some _ | None -> DotDot)
    | Some _ | None -> Dot)
  | '\000' -> Eof
  | _ -> raise Invalid_token

let newline_check tokenizer f =
  let filename, id, line, col = tokenizer.pos in
  if !(tokenizer.src).[id] = '\n' then (
    tokenizer.pos <- (filename, id, line - 1, col);
    f ();
    tokenizer.pos <- (filename, id, line, col))
  else f ()

let next tokenizer : token option =
  let tok = ref None in
  try
    while true do
      match tokenizer.c with
      | Some (' ' | '\n' | '\t' | '\r') -> bump tokenizer
      | Some '0' .. '9' -> (
          let start = tokenizer.pos in
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
            newline_check tokenizer (fun _ ->
                mk_tok tokenizer
                  (Lit (if !is_float then Float else Int))
                  tok start);
            raise Exit)
      | Some ('a' .. 'z' | 'A' .. 'Z' | '_') -> (
          let start = tokenizer.pos in
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
            newline_check tokenizer (fun _ ->
                mk_tok tokenizer
                  (match !buf with
                  | "true" | "false" -> Lit Bool
                  | _ ->
                      if Hashtbl.mem keywords !buf then
                        Hashtbl.find keywords !buf
                      else Ident)
                  tok start);
            raise Exit)
      | Some '"' -> (
          let start = tokenizer.pos in
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
            newline_check tokenizer (fun _ ->
                mk_tok tokenizer (Lit String) tok start);
            raise Exit)
      | Some c ->
          let start = tokenizer.pos in
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
              with I ->
                ();
                mk_tok tokenizer kind tok start)
          | _ ->
              bump tokenizer;
              newline_check tokenizer (fun _ ->
                  mk_tok tokenizer kind tok start));
          raise Exit
      | None ->
          let start = tokenizer.pos in
          mk_tok tokenizer Eof tok start;
          raise Exit
    done;
    !tok
  with Exit -> !tok

let tokenize filename source : tokenizer =
  let tokenizer =
    { c = None; pos = (filename, -1, 1, 0); src = ref source; filename }
  in
  bump tokenizer; tokenizer

let print_all_tokens tokenizer s : tokenizer =
  try
    while true do
      match next tokenizer with
      | Some { kind = Eof; _ } | None -> raise Exit
      | Some token -> display_token token s
    done;
    raise Exit
  with Exit -> tokenize tokenizer.filename s
