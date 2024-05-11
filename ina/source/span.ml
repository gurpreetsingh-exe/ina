type t = {
    lo: int
  ; hi: int
}

let make lo hi = { lo; hi }
let dummy = make 0 0
let from_spans s1 s2 = make s1.lo s2.hi
let range span = span.hi - span.lo
let display_span s = Printf.sprintf "[%d-%d]" s.lo s.hi
