open Printf
(* [%%if profile = "dev"] *)

let display_loc (loc : Printexc.location) =
  Printf.sprintf "%s:%d:%d" loc.filename loc.line_number loc.start_char
;;

let dbg fmt =
  if true
  then (
    let loc () =
      let stack = Printexc.get_callstack 2 in
      let entries = Printexc.raw_backtrace_entries stack in
      let slots =
        Option.get (Printexc.backtrace_slots_of_raw_entry entries.(1))
      in
      let loc = Option.get (Printexc.Slot.location slots.(0)) in
      fprintf stdout "%s:%d:%d: " loc.filename loc.line_number loc.start_char
    in
    (* loc (); *)
    fprintf stdout fmt)
  else fprintf (open_out Filename.null) fmt
;;

let panic _ctx msg : unit = print_endline msg

(* [%%else] *)
(***)
(* let panic _ _ : unit = () *)
(***)
(* [%%endif] *)
