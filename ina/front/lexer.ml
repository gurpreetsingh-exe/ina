open Token
open Source.Span

type lexer = {
    src: string
  ; i: int
  ; size: int
}

let keywords =
  [|
     "fn", Fn
   ; "type", Type
   ; "extern", Extern
   ; "mod", Mod
   ; "let", Let
   ; "using", Using
   ; "if", If
   ; "else", Else
   ; "loop", Loop
   ; "break", Break
   ; "assert", Assert
   ; "as", As
   ; "impl", Impl
   ; "mut", Mut
   ; "match", Match
   ; "true", Lit Bool
   ; "false", Lit Bool
   ; "_", Underscore
  |]
  |> Array.to_seq
  |> Hashtbl.of_seq
;;

let next lx = { lx with i = lx.i + 1 }
let check lx = lx.i < lx.size - 1
let chr ?(n = 0) lx = lx.src.[lx.i + n]
let peek lx = if check lx then Some lx.src.[lx.i + 1] else None

let maybe_multichar = function
  | '=' | '!' | '|' | '&' | '-' | ':' | '.' | '/' -> true
  | _ -> false
;;

let of_char = function
  | ';' -> Semi
  | '(' -> LParen
  | ')' -> RParen
  | '[' -> LBracket
  | ']' -> RBracket
  | '{' -> LBrace
  | '}' -> RBrace
  | '<' -> LAngle
  | '>' -> RAngle
  | '@' -> At
  | ':' -> Colon
  | '=' -> Eq
  | '!' -> Bang
  | ',' -> Comma
  | '/' -> Slash
  | '+' -> Plus
  | '-' -> Minus
  | '*' -> Star
  | '&' -> Ampersand
  | '|' -> Pipe
  | '.' -> Dot
  | '_' -> Underscore
  | '#' -> Pound
  | '\000' -> Eof
  | c ->
      Format.printf "invalid char `%c`\n" c;
      assert false
;;

type tokenchar =
  | Single of token_kind
  | Double of token_kind
  | Triple of token_kind

let of_chars = function
  | '.', '.', '.' -> Triple Dot3
  | '/', '/', '/' -> Triple (Comment (Some Outer))
  | '/', '/', '!' -> Triple (Comment (Some Inner))
  | '.', '.', _ -> Double DotDot
  | '/', '/', _ -> Double (Comment None)
  | '-', '>', _ -> Double Arrow
  | '=', '>', _ -> Double FatArrow
  | '=', '=', _ -> Double EqEq
  | '!', '=', _ -> Double BangEq
  | ':', ':', _ -> Double Colon2
  | '|', '|', _ -> Double Pipe2
  | '&', '&', _ -> Double Ampersand2
  | c, _, _ -> Single (of_char c)
;;

let rec skip lx pred =
  if check lx
  then match chr lx with c when pred c -> skip (next lx) pred | _ -> lx
  else lx
;;

let ident = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let number = function '0' .. '9' -> true | _ -> false

let next lx =
  let rec go lx =
    let lo = lx.i in
    match chr lx with
    | ' ' | '\n' | '\r' | '\t' -> if check lx then go (next lx) else lx, None
    | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
        let lx = skip lx ident in
        let ident = String.sub lx.src lo (lx.i - lo) in
        let kind =
          Hashtbl.find_opt keywords ident |> Option.value ~default:Ident
        in
        lx, Some { kind; span = { lo; hi = lx.i } }
    | '0' .. '9' ->
        let lx = skip lx number in
        (match chr lx, chr ~n:1 lx with
         | '.', '.' -> lx, Some { kind = Lit Int; span = { lo; hi = lx.i } }
         | '.', _ ->
             let lx = next lx in
             let lx = skip lx number in
             lx, Some { kind = Lit Float; span = { lo; hi = lx.i } }
         | _ -> lx, Some { kind = Lit Int; span = { lo; hi = lx.i } })
    | '"' ->
        let lx = next lx in
        let lx = skip lx (( <> ) '"') in
        let lx = next lx in
        lx, Some { kind = Lit String; span = { lo; hi = lx.i } }
    | c when maybe_multichar c ->
        let lx' = next lx in
        let lx'' = next lx' in
        (match of_chars (c, chr ~n:1 lx, chr ~n:2 lx) with
         | Triple (Comment _ as kind) | Double (Comment _ as kind) ->
             let lx = skip lx (( <> ) '\n') in
             lx, Some { kind; span = { lo; hi = lx.i } }
         | Triple kind ->
             next lx'', Some { kind; span = { lo; hi = lo + 3 } }
         | Double kind -> lx'', Some { kind; span = { lo; hi = lo + 2 } }
         | Single kind -> lx', Some { kind; span = { lo; hi = lo + 1 } })
    | c -> next lx, Some { kind = of_char c; span = { lo; hi = lo + 1 } }
  in
  if check lx then go lx else lx, None
;;

let create src = { i = 0; src; size = String.length src }
