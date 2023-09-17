open Printf
open Diagnostic
open Source

type t = {
  sm : Source_map.t option;
  ui_testing : bool;
}

let get_max_line_num_len ms : int =
  List.fold_left
    (fun max_len next -> max next max_len)
    0
    (List.map (fun Span.{ hi; _ } -> hi) ms.primary_spans)

let emit emitter diagnostic =
  let curr_line = ref 0 in
  let buf = Styled_buffer.{ lines = [||] } in
  let anon = emitter.ui_testing in
  let colors = not anon in
  Styled_buffer.append buf ~line:!curr_line
    (display_level diagnostic.level)
    (Level diagnostic.level);
  Styled_buffer.append buf ~line:!curr_line
    (": " ^ diagnostic.message)
    Header;
  incr curr_line;
  if has_primary_span diagnostic.span then (
    match emitter.sm with
    | Some sm ->
        let prim_span = primary_span diagnostic.span in
        let file = Source_map.lookup_file sm prim_span.lo in
        let max_line_num =
          if anon then 2
          else (
            let off = get_max_line_num_len diagnostic.span in
            let line, _ = Source_map.File.lookup_file_pos file off in
            String.length @@ string_of_int line)
        in
        let pad = String.make (max_line_num + 1) ' ' in
        let line, col = Source_map.File.lookup_file_pos file prim_span.lo in
        Styled_buffer.append buf ~line:!curr_line (pad ^ "--> ") LineNum;
        Styled_buffer.append buf ~line:!curr_line
          (sprintf "%s:%d:%d"
             (if anon then Filename.basename file.name else file.name)
             (line + 1) (col + 1))
          LineCol;
        incr curr_line;
        Styled_buffer.append buf ~line:!curr_line (pad ^ " |") LineNum;
        incr curr_line;
        let src = Source_map.File.lookup_line_src file line in
        let pad = String.make (max_line_num + 1) ' ' in
        let ln = if anon then "LL" else string_of_int (line + 1) in
        Styled_buffer.append buf ~line:!curr_line (pad ^ " | ") LineNum;
        Styled_buffer.puts buf !curr_line 1 ln LineNum;
        Styled_buffer.append buf ~line:!curr_line src NoStyle;
        Styled_buffer.append buf ~line:!curr_line (pad ^ " |") LineNum;
        Styled_buffer.append buf ~line:!curr_line
          (String.make (col + 1) ' ')
          NoStyle;
        Styled_buffer.append buf ~line:!curr_line
          (String.make (prim_span.hi - prim_span.lo) '^')
          (Level diagnostic.level);
        List.iter
          (fun (_span, msg, is_prim) ->
            if is_prim then
              Styled_buffer.append buf ~line:!curr_line (" " ^ msg)
                (Level Err))
          diagnostic.span.labels;
        incr curr_line
    | None -> ())
  else ();
  eprintf "%s\n\n" (Styled_buffer.render buf colors);
  flush stderr
