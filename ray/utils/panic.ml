open Errors

[%%if profile = "dev"]

let display_loc (loc : Printexc.location) =
  Printf.sprintf "%s:%d:%d" loc.filename loc.line_number loc.start_char

let panic _ctx msg : unit =
  let stack = Printexc.get_callstack 2 in
  let entries = Printexc.raw_backtrace_entries stack in
  let slots =
    Option.get (Printexc.backtrace_slots_of_raw_entry entries.(1))
  in
  let loc_ = Option.get (Printexc.Slot.location slots.(0)) in
  (* Token. *)
  (*   { *)
  (*     start = (loc.filename, 0, loc.line_number, loc.start_char); *)
  (*     ending = ("", 0, loc.line_number, 0); *)
  (*   } *)
  let dg =
    Diagnostic.
      {
        level = Bug;
        message = msg;
        span = { primary_spans = []; labels = [] };
        children = [];
        sugg = [];
        loc =
          {
            line = loc_.line_number;
            col = loc_.start_char;
            file = loc_.filename;
          };
      }
  in
  let ic = open_in loc_.filename in
  let _s = really_input_string ic (in_channel_length ic) in
  let emitter = Emitter.{ sm = None; ui_testing = false } in
  Emitter.emit emitter dg

[%%else]

let panic _ _ : unit = ()

[%%endif]
