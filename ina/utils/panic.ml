open Printf
(* [%%if profile = "dev"] *)

let display_loc (loc : Printexc.location) =
  Printf.sprintf "%s:%d:%d" loc.filename loc.line_number loc.start_char
;;

let devnull = open_out Filename.null

let dbg fmt =
  if false
  then
    let _loc () =
      let stack = Printexc.get_callstack 2 in
      let entries = Printexc.raw_backtrace_entries stack in
      let slots =
        Option.get (Printexc.backtrace_slots_of_raw_entry entries.(1))
      in
      let loc = Option.get (Printexc.Slot.location slots.(0)) in
      fprintf stdout "%s:%d:%d: " loc.filename loc.line_number loc.start_char
    in
    (* loc (); *)
    fprintf stdout fmt
  else fprintf devnull fmt
;;

let panic _ctx msg : unit = print_endline msg

(* [%%else] *)
(***)
(* let panic _ _ : unit = () *)
(***)
(* [%%endif] *)
