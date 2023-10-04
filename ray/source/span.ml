type t = {
    lo: int
  ; hi: int
}

let make lo hi = { lo; hi }
let display_span s = Printf.sprintf "[%d-%d]" s.lo s.hi
