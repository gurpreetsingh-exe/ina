open Printf
open Token

let e = "\x1b[0m"

type level =
  | Note
  | Warn
  | Err
  | Bug

let level_to_color = function
  | Note -> "\x1b[1;36m"
  | Warn -> "\x1b[1;33m"
  | Err | Bug -> "\x1b[1;31m"

let display_level = function
  | Note -> "note"
  | Warn -> "warning"
  | Err -> "error"
  | Bug -> "bug"

let render_level level =
  sprintf "%s%s%s" (level_to_color level) (display_level level) e

type multi_span = {
  primary_spans : span list;
  labels : (span * string * bool) list;
}

let primary_span ms = List.hd ms.primary_spans

let get_max_line_num_len ms : int =
  String.length
    (string_of_int
       (List.fold_left
          (fun max_len next -> max next max_len)
          0
          (List.map (fun { ending = _, _, l, _; _ } -> l) ms.primary_spans)))

type substitution = { parts : (span * string) list }

type sugg = {
  message : string;
  sub : substitution list;
}

type sub_diagnostic = {
  level : level;
  message : string;
  span : multi_span;
}

type diagnostic_loc = {
  line : int;
  col : int;
  file : string;
}

let dg_loc_from_span (span : span) =
  let file, _, line, col = span.start in
  { line; col; file }

let render_dg_loc loc anon_file =
  if anon_file then
    sprintf "%s:%d:%d" (Filename.basename loc.file) loc.line loc.col
  else sprintf "%s:%d:%d" loc.file loc.line loc.col

type t = {
  level : level;
  message : string;
  span : multi_span;
  children : sub_diagnostic list;
  sugg : sugg list;
  loc : diagnostic_loc;
}
