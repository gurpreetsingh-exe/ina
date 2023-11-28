let time (f : unit -> 'a) : float * 'a =
  let start = Sys.time () in
  (Sys.time () -. start) *. 1000., f ()
;;
