type literal =
  | Int
  | Float
  | Char
  | Bool
  | String

let display_literal = function
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Char -> "char"
  | Bool -> "bool"

type comment_style =
  | Inner
  | Outer

let display_comment_style = function Inner -> "inner" | Outer -> "outer"

type token_kind =
  | Fn
  | Extern
  | Ident
  | Lit of literal
  | Comment of comment_style option
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

let display_token_kind = function
  | Fn -> "fn"
  | Extern -> "extern"
  | Ident -> "identifier"
  | Lit lit -> display_literal lit
  | Comment (Some kind) -> "comment " ^ display_comment_style kind
  | Comment None -> "comment"
  | Semi -> ";"
  | LParen -> "("
  | RParen -> ")"
  | LBrace -> "{"
  | RBrace -> "}"
  | LBracket -> "["
  | RBracket -> "]"
  | Colon -> ":"
  | Eq -> "="
  | Comma -> ","
  | Slash -> "/"
  | Eof -> "eof"

type pos = string * int

type span = {
  start : pos;
  ending : pos;
}

type token = {
  kind : token_kind;
  span : span;
}

let display_token t s =
  let { kind; span = { start = _, st; ending = _, e } } = t in
  Printf.printf "%s: %s\n" (display_token_kind kind)
    (String.sub s st (e - st))

let get_token_str t s : string =
  let { span = { start = _, st; ending = _, e }; _ } = t in
  String.sub s st (e - st)
