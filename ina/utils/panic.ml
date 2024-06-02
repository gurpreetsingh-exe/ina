open Printf
(* [%%if profile = "dev"] *)

let trace () =
  let stack = Printexc.get_callstack 10 in
  let slots = Printexc.backtrace_slots stack in
  match slots with
  | Some slots ->
      Array.iter
        (fun slot ->
          Printexc.Slot.format 0 slot
          |> function Some fmt -> printf "  %s\n" fmt | _ -> ())
        slots
  | None -> ()
;;

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
