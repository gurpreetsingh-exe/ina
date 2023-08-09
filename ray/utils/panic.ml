open Errors

[%%if profile = "dev"]

let display_loc (loc : Printexc.location) =
  Printf.sprintf "%s:%d:%d" loc.filename loc.line_number loc.start_char

let panic ctx msg : unit =
  let stack = Printexc.get_callstack 2 in
  let entries = Printexc.raw_backtrace_entries stack in
  let slots =
    Option.get (Printexc.backtrace_slots_of_raw_entry entries.(1))
  in
  let loc = Option.get (Printexc.Slot.location slots.(0)) in
  let span =
    Token.
      {
        start = (loc.filename, 0, loc.line_number, loc.start_char);
        ending = ("", 0, loc.line_number, 0);
      }
  in
  let dg =
    Diagnostic.
      {
        level = Bug;
        message = msg;
        span = { primary_spans = [span]; labels = [] };
        children = [];
        sugg = [];
        loc =
          {
            line = loc.line_number;
            col = loc.start_char;
            file = loc.filename;
          };
      }
  in
  let ic = open_in loc.filename in
  let s = really_input_string ic (in_channel_length ic) in
  let emitter =
    Emitter.{ ctx; source = Array.of_list (String.split_on_char '\n' s) }
  in
  Emitter.emit emitter dg

[%%else]

let panic _ _ : unit = ()

[%%endif]
