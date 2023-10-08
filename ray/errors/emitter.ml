open Printf
open Diagnostic
open Source.Source_map
open Styled_buffer

type t = {
    sm: source_map option
  ; ui_testing: bool
}

class emitter sm ui_testing =
  object (self)
    val sm : source_map option = sm
    val ui_testing : bool = ui_testing

    (* public methods *)
    method emit_diagnostic (diag : diagnostic) =
      let max_line_num_len = 2 in
      self#emit_messages diag#span diag#message diag#level max_line_num_len

    (* private methods *)
    method private emit_messages span msg level max_line_num_len =
      let ( += ) (x : int ref) (y : int) = x := !x + y in
      let buf = new styled_buffer in
      (if (not span#has_primary_span) && not span#has_span_labels
       then
         for _ = 0 to max_line_num_len do
           buf#prepend 0 " " NoStyle
         done
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
             List.iteri f lines));
      self#emit_to_destination buf

    (* method private msg_to_buffer *)
    (*     (buf : styled_buffer) *)
    (*     (msg : message vec) *)
    (*     pad = *)
    (*   let padding = String.make (pad + 5) ' ' in *)
    (*   let line_no = ref 0 in *)
    (*   msg#iter (fun m -> *)
    (*       let lines = String.split_on_char '\n' m.msg in *)
    (*       let f i line = *)
    (*         if i <> 0 *)
    (*         then ( *)
    (*           incr line_no; *)
    (*           buf#append !line_no padding NoStyle); *)
    (*         buf#append !line_no line NoStyle *)
    (*       in *)
    (*       if List.length lines > 1 *)
    (*       then List.iteri f lines *)
    (*       else buf#append !line_no m.msg NoStyle) *)

    method private emit_to_destination (buf : styled_buffer) =
      let styled_string = buf#render in
      styled_string#iter (fun line ->
          line#iter (fun part ->
              let col =
                match part.style with
                | Level level -> Diagnostic.level_to_color level
                | Header | LineCol -> "\x1b[1m"
                | LineNum -> "\x1b[1;34m"
                | NoStyle -> ""
              in
              fprintf stderr "%s%s%s" col part.text e);
          fprintf stderr "\n";
          flush stdout)
  end
