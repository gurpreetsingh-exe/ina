open Printf

type style =
  | LineCol
  | LineNum
  | NoStyle
  | Header
  | Level of Diagnostic.level

type styled_char = {
    mutable chr: char
  ; mutable style: style
}

type t = { mutable lines: styled_char array array }

let ensure_lines buf line =
  if line >= Array.length buf.lines
  then (
    let tmp = buf.lines in
    buf.lines <- Array.make (line + 1) [||];
    Array.blit tmp 0 buf.lines 0 (Array.length tmp))
;;

let putc (buf : t) (line : int) (col : int) (chr : char) (style : style) =
  ensure_lines buf line;
  if col >= Array.length buf.lines.(line)
  then (
    let tmp = buf.lines.(line) in
    buf.lines.(line) <- Array.make (col + 1) { chr = ' '; style = NoStyle };
    Array.blit tmp 0 buf.lines.(line) 0 (Array.length tmp));
  buf.lines.(line).(col) <- { chr; style }
;;

let puts (buf : t) (line : int) (col : int) (string : string) (style : style)
  =
  String.iteri (fun n c -> putc buf line (col + n) c style) string
;;

let append (buf : t) ~(line : int) (str : string) (style : style) =
  if line >= Array.length buf.lines
  then puts buf line 0 str style
  else
    let col = Array.length buf.lines.(line) in
    puts buf line col str style
;;

let render (buf : t) colors =
  let f chr =
    if colors
    then
      let col =
        match chr.style with
        | Level level -> Diagnostic.level_to_color level
        | Header | LineCol -> "\x1b[1m"
        | LineNum -> "\x1b[1;34m"
        | NoStyle -> ""
      in
      sprintf "%s%c\x1b[0m" col chr.chr
    else sprintf "%c" chr.chr
  in
  let g line = String.concat "" (Array.to_list (Array.map f line)) in
  String.concat "\n" (Array.to_list (Array.map g buf.lines))
;;
