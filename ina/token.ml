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
  | Import
  | Unit
  | If
  | Else
  | Assert
  | As
  | Impl
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
  | Dot
  | DotDot
  | Dot3
  | Eof

let display_token_kind = function
  | Fn -> "fn"
  | Type -> "type"
  | Extern -> "extern"
  | Mod -> "mod"
  | Let -> "let"
  | Import -> "import"
  | Unit -> "unit"
  | If -> "if"
  | Else -> "else"
  | Assert -> "assert"
  | As -> "as"
  | Impl -> "impl"
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
  | Dot -> "."
  | DotDot -> ".."
  | Dot3 -> "..."
  | Eof -> "eof"
;;

(* let display_span span = *)
(*   let { start = file, _, l, c; _ } = span in *)
(*   Printf.sprintf "%s:%d:%d" file l c *)

type token = {
    kind: token_kind
  ; span: Span.t
}

let display_token t s =
  let { kind; span = { lo; hi } } = t in
  Printf.printf "%10s: %10s\n" (display_token_kind kind) (String.sub s lo hi)
;;

(* let { kind; span = { start = _, st, l, c; ending = _, e, _, _ } } = t in *)
(* Printf.printf "[%3d: %3d] [%3d: %3d] %10s: %10s\n" st e l c *)
(*   (display_token_kind kind) *)
(*   (String.sub s st (e - st)) *)

let get_token_str t s =
  let { span = { lo; hi }; _ } = t in
  String.sub s lo (hi - lo)
;;

(* let get_token_str t s : string = *)
(*   let { span = { start = _, st, _, _; ending = _, e, _, _ }; _ } = t in *)
(*   String.sub s st (e - st) *)