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

let get_span_line (pos : Token.pos) : int =
  let _, _, line, _ = pos in
  line

let get_span_end (span : Token.span) : int =
  let Token.{ ending = _, _, line, _; _ } = span in
  line

let get_span_col (span : Token.span) : int =
  let Token.{ start = _, _, _, col; _ } = span in
  col

let spread (span : Token.span) : int =
  let Token.{ start = _, s, _, _; ending = _, e, _, _ } = span in
  e - s

let extract_span_line (span : Token.span) (s : string array) : string =
  s.(get_span_line span.start - 1)

let add_annotation (buf : Styled_buffer.t) (buf_line : int ref)
    (span : Token.span) level max_line_num_len =
  let pad = String.make (max_line_num_len + 1) ' ' in
  Styled_buffer.append buf ~line:!buf_line (pad ^ " | ") LineNum;
  let line_off = get_span_line span.start in
  let span_lines = get_span_end span - line_off in
  let col = get_span_col span - 1 in
  let str = String.make col ' ' in
  let len = spread span in
  if span_lines <> 0 then ()
    (* let underline = String.make 1 '^' in *)
    (* Styled_buffer.append buf ~line:!buf_line (str ^ underline ^ " ") level) *)
  else (
    let underline = String.make len '^' in
    Styled_buffer.append buf ~line:!buf_line (str ^ underline) level)

let render_col_sep (buf : Styled_buffer.t) (buf_line : int ref)
    max_line_num_len =
  let pad = String.make (max_line_num_len + 1) ' ' in
  Styled_buffer.append buf ~line:!buf_line (pad ^ " | ") LineNum

let render_source_line emitter buf buf_line line_no max_line_num_len src =
  let pad = String.make (max_line_num_len + 1) ' ' in
  let ln =
    if emitter.ctx.options.ui_testing then "LL" else string_of_int line_no
  in
  Styled_buffer.append buf ~line:!buf_line (pad ^ " | ") LineNum;
  Styled_buffer.puts buf !buf_line 1 ln LineNum;
  Styled_buffer.append buf ~line:!buf_line src NoStyle;
  incr buf_line

let render_source_lines emitter (buf : Styled_buffer.t)
    (max_line_num_len : int) (line : int ref) (span : Token.span)
    (s : string array) =
  let line_off = get_span_line span.start in
  let span_lines = get_span_end span - line_off in
  (* printf "%d:%d\n" line_off (get_span_end span); *)
  if span_lines = 0 then (
    let start_line = s.(line_off - 1) in
    render_source_line emitter buf line line_off max_line_num_len start_line)
  else if span_lines = 1 then (
    let start_line = s.(line_off - 1) in
    render_source_line emitter buf line line_off max_line_num_len start_line;
    let end_line = s.(line_off + span_lines - 1) in
    render_source_line emitter buf line (line_off + span_lines)
      max_line_num_len end_line)
  else (
    let start_line = s.(line_off - 1) in
    render_source_line emitter buf line line_off max_line_num_len start_line;
    render_col_sep buf line max_line_num_len;
    Styled_buffer.puts buf !line 1 "..." LineNum;
    incr line;
    let end_line = s.(line_off + span_lines - 1) in
    render_source_line emitter buf line (line_off + span_lines)
      max_line_num_len end_line)
(* for i = 0 to span_lines do *)
(*   let src_line = s.(line_off + i - 1) in *)
(*   render_source_line emitter buf line (line_off + i) max_line_num_len *)
(*     src_line *)
(* done *)

let render_secondary_labels buf ms =
  List.iter
    (fun (_, msg, is_prim) ->
      if not is_prim then Styled_buffer.append buf ~line:1 msg (Level Note))
    ms.labels

let emit emitter diagnostic =
  let curr_line = ref 0 in
  let buf = Styled_buffer.{ lines = [||] } in
  let anon_loc = emitter.ctx.options.ui_testing in
  let max_line_num_len, colors =
    if emitter.ctx.options.ui_testing then (2, false)
    else (get_max_line_num_len diagnostic.span, true)
  in
  let pad = String.make (max_line_num_len + 1) ' ' in
  Styled_buffer.append buf ~line:!curr_line
    (display_level diagnostic.level)
    (Level diagnostic.level);
  Styled_buffer.append buf ~line:!curr_line
    (": " ^ diagnostic.message)
    Header;
  incr curr_line;
  Styled_buffer.append buf ~line:!curr_line (pad ^ "--> ") LineNum;
  Styled_buffer.append buf ~line:!curr_line
    (Diagnostic.render_dg_loc diagnostic.loc anon_loc)
    LineCol;
  incr curr_line;
  Styled_buffer.append buf ~line:!curr_line (pad ^ " |") LineNum;
  incr curr_line;
  let prim_span = primary_span diagnostic.span in
  render_source_lines emitter buf max_line_num_len curr_line prim_span
    emitter.source;
  add_annotation buf curr_line prim_span (Level diagnostic.level)
    max_line_num_len;
  List.iter
    (fun ((span : Token.span), msg, is_prim) ->
      let line_off = get_span_line span.start in
      let span_lines = get_span_end span - line_off in
      if span_lines = 0 && is_prim then
        Styled_buffer.append buf ~line:!curr_line (" " ^ msg) (Level Err))
    diagnostic.span.labels;
  eprintf "%s\n\n" (Styled_buffer.render buf colors);
  flush stderr
