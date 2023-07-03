open Printf
open Diagnostic
open Session

type t = {
  ctx : Context.t;
  source : string array;
}

let get_span_str (span : Token.span) (s : string array) : string =
  let Token.{ start = _, _, line, st; ending = _, _, _, e } = span in
  String.sub s.(line) st (e - st)

let get_span_line (span : Token.span) : int =
  let Token.{ start = _, _, line, _; _ } = span in
  line

let get_span_col (span : Token.span) : int =
  let Token.{ start = _, _, _, col; _ } = span in
  col

let extract_span_line (span : Token.span) (s : string array) : string =
  s.(get_span_line span - 1)

let add_annotation (buf : Styled_buffer.t) (span : Token.span) =
  let col = get_span_col span - 1 in
  let str = String.make col ' ' in
  Styled_buffer.append buf ~line:4 (str ^ "^ ") (Level Err)

let draw_col_sep (buf : Styled_buffer.t) (line : int) (col : int) =
  Styled_buffer.puts buf line col "| " LineNum

let render_source_line emitter (buf : Styled_buffer.t) (line : int)
    (line_off : int) (s : string array) =
  let src_line = s.(line_off - 1) in
  Styled_buffer.append buf ~line src_line NoStyle;
  let ln =
    if emitter.ctx.options.ui_testing then "LL" else string_of_int line_off
  in
  Styled_buffer.puts buf line 1 ln LineNum

let emit emitter diagnostic =
  let buf = Styled_buffer.{ lines = [||] } in
  Styled_buffer.append buf ~line:0
    (display_level diagnostic.level)
    (Level diagnostic.level);
  Styled_buffer.append buf ~line:0 (": " ^ diagnostic.message) Header;
  Styled_buffer.append buf ~line:1
    ("  " ^ Diagnostic.render_dg_loc diagnostic.loc)
    LineCol;
  let max_line_num_len =
    if emitter.ctx.options.ui_testing then 2
    else get_max_line_num_len diagnostic.span
  in
  let pad = String.make (max_line_num_len + 1) ' ' in
  Styled_buffer.append buf ~line:2 (pad ^ " | ") LineNum;
  Styled_buffer.append buf ~line:3 (pad ^ " | ") LineNum;
  let prim_span = primary_span diagnostic.span in
  render_source_line emitter buf 3 (get_span_line prim_span) emitter.source;
  Styled_buffer.append buf ~line:4 (pad ^ " | ") LineNum;
  add_annotation buf prim_span;
  List.iter
    (fun (_, msg, is_prim) ->
      if is_prim then Styled_buffer.append buf ~line:4 msg (Level Err)
      else Styled_buffer.append buf ~line:4 msg (Level Note))
    diagnostic.span.labels;
  eprintf "%s\n\n" (Styled_buffer.render buf)
