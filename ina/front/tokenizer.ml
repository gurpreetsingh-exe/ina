open Token

type tokenizer = {
    mutable c: char option
  ; src: string ref
  ; mutable pos: int
  ; filename: string
}

let keywords =
  [|
     "fn", Fn
   ; "type", Type
   ; "extern", Extern
   ; "mod", Mod
   ; "let", Let
   ; "import", Import
   ; "if", If
   ; "else", Else
   ; "assert", Assert
   ; "as", As
   ; "impl", Impl
   ; "mut", Mut
  |]
  |> Array.to_seq
  |> Hashtbl.of_seq
;;

let mk_tok
    (tokenizer : tokenizer)
    (kind : token_kind)
    (tok : token option ref)
    (start : int)
  =
  tok := Some { kind; span = { lo = start; hi = tokenizer.pos } }
;;

let bump tokenizer =
  if tokenizer.pos < String.length !(tokenizer.src) - 1
  then (
    tokenizer.pos <- tokenizer.pos + 1;
    let c = !(tokenizer.src).[tokenizer.pos] in
    tokenizer.c <- Some c)
  else tokenizer.c <- None
;;

let peek tokenizer =
  if tokenizer.pos < String.length !(tokenizer.src) - 1
  then Some !(tokenizer.src).[tokenizer.pos + 1]
  else None
;;

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
  | '<' -> LAngle
  | '>' -> RAngle
  | ':' ->
      (match peek tokenizer with
       | Some ':' ->
           bump tokenizer;
           Colon2
       | Some _ | None -> Colon)
  | '=' ->
      (match peek tokenizer with
       | Some '=' ->
           bump tokenizer;
           EqEq
       | Some _ | None -> Eq)
  | '!' ->
      (match peek tokenizer with
       | Some '=' ->
           bump tokenizer;
           BangEq
       | Some _ | None -> Bang)
  | ',' -> Comma
  | '/' ->
      (match peek tokenizer with
       | Some '/' ->
           bump tokenizer;
           Comment
             (match peek tokenizer with
              | Some '/' -> Some Outer
              | Some '!' -> Some Inner
              | Some _ | None -> None)
       | Some _ | None -> Slash)
  | '+' -> Plus
  | '-' ->
      (match peek tokenizer with
       | Some '>' ->
           bump tokenizer;
           Arrow
       | Some _ | None -> Minus)
  | '*' -> Star
  | '|' ->
      (match peek tokenizer with
       | Some '|' ->
           bump tokenizer;
           Pipe2
       | Some _ | None -> Pipe)
  | '&' ->
      (match peek tokenizer with
       | Some '&' ->
           bump tokenizer;
           Ampersand2
       | Some _ | None -> Ampersand)
  | '.' ->
      (match peek tokenizer with
       | Some '.' ->
           bump tokenizer;
           (match peek tokenizer with
            | Some '.' ->
                bump tokenizer;
                Dot3
            | Some _ | None -> DotDot)
       | Some _ | None -> Dot)
  | '\000' -> Eof
  | _ -> raise Invalid_token
;;

let next tokenizer : token option =
  let tok = ref None in
  try
    while true do
      match tokenizer.c with
      | Some (' ' | '\n' | '\t' | '\r') -> bump tokenizer
      | Some '0' .. '9' ->
          let start = tokenizer.pos in
          let exception I in
          let is_float = ref false in
          let buf : string ref = ref "" in
          (try
             while true do
               match tokenizer.c with
               | Some (('0' .. '9' | '.') as c) ->
                   buf := !buf ^ String.make 1 c;
                   bump tokenizer;
                   if (not !is_float) && c == '.' then is_float := true
               | Some _ | None -> raise I
             done
           with
           | I ->
               mk_tok
                 tokenizer
                 (Lit (if !is_float then Float else Int))
                 tok
                 start;
               raise Exit)
      | Some ('a' .. 'z' | 'A' .. 'Z' | '_') ->
          let start = tokenizer.pos in
          let buf = ref "" in
          let exception I in
          (try
             while true do
               match tokenizer.c with
               | Some (('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') as c) ->
                   buf := !buf ^ String.make 1 c;
                   bump tokenizer
               | Some _ | None -> raise I
             done
           with
           | I ->
               mk_tok
                 tokenizer
                 (match !buf with
                  | "true" | "false" -> Lit Bool
                  | _ ->
                      if Hashtbl.mem keywords !buf
                      then Hashtbl.find keywords !buf
                      else Ident)
                 tok
                 start;
               raise Exit)
      | Some '"' ->
          let start = tokenizer.pos in
          bump tokenizer;
          let exception I in
          (try
             while true do
               match tokenizer.c with
               | Some '"' ->
                   bump tokenizer;
                   raise I
               | Some _ -> bump tokenizer
               | None -> raise I
             done
           with
           | I ->
               mk_tok tokenizer (Lit String) tok start;
               raise Exit)
      | Some c ->
          let start = tokenizer.pos in
          let kind = get_token_type c tokenizer in
          (match kind with
           (* TODO: strip the `//*` from comments *)
           | Comment _ ->
               let exception I in
               (try
                  while true do
                    match tokenizer.c with
                    | Some '\n' | None -> raise I
                    | Some _ -> bump tokenizer
                  done
                with
                | I ->
                    ();
                    mk_tok tokenizer kind tok start)
           | _ ->
               bump tokenizer;
               mk_tok tokenizer kind tok start);
          raise Exit
      | None ->
          let start = tokenizer.pos in
          mk_tok tokenizer Eof tok start;
          raise Exit
    done;
    !tok
  with
  | Exit -> !tok
;;

let tokenize filename source : tokenizer =
  let tokenizer = { c = None; pos = -1; src = ref source; filename } in
  bump tokenizer;
  tokenizer
;;

let print_all_tokens tokenizer s : tokenizer =
  try
    while true do
      match next tokenizer with
      | Some { kind = Eof; _ } | None -> raise Exit
      | Some token -> display_token token s
    done;
    raise Exit
  with
  | Exit -> tokenize tokenizer.filename s
;;
