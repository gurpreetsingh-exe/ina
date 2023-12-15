open Printf
open Diagnostic
open Source.Source_map
open Styled_buffer
open Structures.Vec

type t = {
    sm: source_map option
  ; ui_testing: bool
}

let repeat n c = String.make n c
let empty n = repeat n ' '

class emitter sm ui_testing =
  object (self)
    val sm : source_map option = sm
    val ui_testing : bool = ui_testing

    (* public methods *)
    method emit_diagnostic (diag : diagnostic) =
      let max_line_num_len =
        if ui_testing
        then 2
        else string_of_int @@ self#max_line_num diag#span |> String.length
      in
      self#emit_messages diag#span diag#message diag#level max_line_num_len;
      diag#children#iter self#emit_diagnostic;
      fprintf stderr "\n";
      flush stderr

    method max_line_num (ms : multi_span) =
      let open Source.Span in
      match sm with
      | Some sm ->
          let find_max acc span =
            let file = sm#lookup_file span.hi in
            let line, _ = file#lookup_file_pos span.hi in
            max (line + 1) acc
          in
          map ms#labels (fun (span, _) -> span)
          |> (fold_left find_max 0 ms#primary_spans |> fold_left find_max)
      | None -> 0

    (* private methods *)
    method private emit_messages span msg level max_line_num_len =
      let ( += ) (x : int ref) (y : int) = x := !x + y in
      let buf = new styled_buffer in
      (if (not span#has_primary_span) && not span#has_span_labels
       then (
         buf#append 0 (empty (max_line_num_len + 2)) NoStyle;
         buf#append 0 "= " LineNum;
         buf#append 0 (render_level level) (Level level);
         buf#append 0 ": " Header;
         self#msg_to_buffer buf msg (max_line_num_len + 1) "note";
         ())
       else
         let label_width = ref 0 in
         let level_str = render_level level in
         buf#append 0 level_str (Level level);
         buf#append 0 ": " Header;
         label_width += (String.length (display_level level) + 2);
         let sp = Option.get span#primary_span in
         (match sm with
          | Some sm ->
              let loc = sm#span_to_string sp.lo in
              buf#append 0 loc Header;
              buf#append 0 ": " Header;
              label_width += (String.length loc + 2)
          | None -> ());
         msg#iter (fun m ->
             let lines = String.split_on_char '\n' m.msg in
             let f i line =
               let msg =
                 sprintf
                   "%s%s"
                   (if i = 0
                    then String.empty
                    else String.make !label_width ' ')
                   line
               in
               buf#append i msg Header
             in
             List.iteri f lines);
         let i = ref 1 in
         let emit_label sm sp =
           let open Source.Span in
           let file = sm#lookup_file sp.hi in
           (*
            * LL |
            * ^^    <--- max_line_num_len
            *^  ^^  <--- extra 3 for spacing and bar
            *)
           let emit_sidebar ?(newline = true) () =
             let prefix = empty max_line_num_len in
             buf#append !i (" " ^ prefix ^ " |") LineNum;
             if newline then i += 1
           in
           let emit_sidebar_with_line line_no =
             emit_sidebar ~newline:false ();
             buf#puts !i 1 (string_of_int (line_no + 1)) LineNum
           in
           emit_sidebar ();
           let line_no, col = sm#lookup_line_pos sp.lo in
           emit_sidebar_with_line line_no;
           let line = file#lookup_line_src line_no in
           buf#append !i (" " ^ line) NoStyle;
           emit_sidebar ~newline:false ();
           buf#append !i (empty col) NoStyle;
           let underline = repeat (sp.hi - sp.lo) '^' in
           buf#append !i (" " ^ underline) (Level level)
         in
         match sm with
         | Some sm ->
             (match span#labels#first with
              | Some (sp, msg) ->
                  emit_label sm sp;
                  buf#append !i ("  " ^ msg.msg) (Level level)
              | None -> emit_label sm sp)
         | None -> ());
      self#emit_to_destination level buf

    method private msg_to_buffer buf msg padding label =
      let padding = String.make (padding + String.length label + 5) ' ' in
      let line_no = ref 0 in
      msg#iter (fun m ->
          let lines = String.split_on_char '\n' m.msg in
          let f i line =
            if i <> 0
            then (
              incr line_no;
              buf#append !line_no padding NoStyle);
            buf#append !line_no line NoStyle
          in
          if List.length lines > 1
          then List.iteri f lines
          else buf#append !line_no m.msg NoStyle)

    method private emit_to_destination level buf =
      let styled_string = buf#render in
      styled_string#iteri (fun i line ->
          line#iter (fun part ->
              let col =
                match part.style with
                | Level level -> Diagnostic.level_to_color level
                | Header | LineCol -> "\x1b[1m"
                | LineNum -> "\x1b[1;34m"
                | NoStyle -> ""
              in
              fprintf stderr "%s%s%s" col part.text e);
          match level with
          | (Note | Help) when i = styled_string#len - 1 -> ()
          | _ -> fprintf stderr "\n")
  end
