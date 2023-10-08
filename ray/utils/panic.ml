(* [%%if profile = "dev"] *)

let display_loc (loc : Printexc.location) =
  Printf.sprintf "%s:%d:%d" loc.filename loc.line_number loc.start_char
;;

let panic _ctx msg : unit = print_endline msg

(* [%%else] *)
(***)
(* let panic _ _ : unit = () *)
(***)
(* [%%endif] *)
