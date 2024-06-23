open Source

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
;;

type comment_style =
  | Inner
  | Outer

let display_comment_style = function Inner -> "inner" | Outer -> "outer"

type token_kind =
  | Fn
  | Type
  | Extern
  | Mod
  | Let
  | Using
  | If
  | Else
  | Loop
  | Break
  | Assert
  | As
  | Impl
  | Mut
  | Match
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
  | LAngle
  | RAngle
  | Colon
  | Colon2
  | Eq
  | EqEq
  | Bang
  | BangEq
  | Comma
  | Plus
  | Minus
  | Star
  | Slash
  | Pipe
  | Pipe2
  | Ampersand
  | Ampersand2
  | Arrow
  | FatArrow
  | Dot
  | DotDot
  | Dot3
  | At
  | Underscore
  | Pound
  | Eof

let display_token_kind = function
  | Fn -> "fn"
  | Type -> "type"
  | Extern -> "extern"
  | Mod -> "mod"
  | Let -> "let"
  | Using -> "using"
  | If -> "if"
  | Else -> "else"
  | Loop -> "loop"
  | Break -> "break"
  | Assert -> "assert"
  | As -> "as"
  | Impl -> "impl"
  | Mut -> "mut"
  | Match -> "match"
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
  | LAngle -> "<"
  | RAngle -> ">"
  | Colon -> ":"
  | Colon2 -> "::"
  | Eq -> "="
  | EqEq -> "=="
  | Bang -> "!"
  | BangEq -> "!="
  | Comma -> ","
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Pipe -> "|"
  | Pipe2 -> "||"
  | Ampersand -> "&"
  | Ampersand2 -> "&&"
  | Arrow -> "->"
  | FatArrow -> "=>"
  | Dot -> "."
  | DotDot -> ".."
  | Dot3 -> "..."
  | At -> "@"
  | Underscore -> "_"
  | Pound -> "#"
  | Eof -> "eof"
;;

type token = {
    kind: token_kind
  ; span: Span.t
}

let can_begin_expr = function
  | Ident | If | Loop | Break | Match | Underscore | LBrace | LBracket
  | LParen | Lit _ | Ampersand | Ampersand2 | Star | Pound ->
      true
  | _ -> false
;;

let display_token t s =
  let { kind; span = { lo; hi } } = t in
  Printf.printf
    "%10s: %10s\n"
    (display_token_kind kind)
    (String.sub s lo (hi - lo))
;;

let get_token_str t s =
  let { span = { lo; hi }; _ } = t in
  String.sub s lo (hi - lo)
;;
