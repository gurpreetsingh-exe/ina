let green ?(bold = true) s =
  (if bold then "\x1b[1;32m" else "\x1b[32m") ^ s ^ "\x1b[0m"
;;

let red ?(bold = true) s =
  (if bold then "\x1b[1;31m" else "\x1b[31m") ^ s ^ "\x1b[0m"
;;

let cyan ?(bold = true) s =
  (if bold then "\x1b[1;36m" else "\x1b[36m") ^ s ^ "\x1b[0m"
;;

let q s = "'" ^ s ^ "'"
let t = green "├─"
let bar = green "│ "
let l = green "└─"

let render_children ?(prefix = "") ?(skip_last = false) printer items f =
  let nitems = items#len in
  let g i item =
    let last = i = nitems - 1 in
    printer#append prefix;
    printer#append (if last && not skip_last then l else t);
    f item (prefix ^ if last && not skip_last then "  " else bar)
  in
  items#iteri g
;;

let render_child ?(prefix = "") printer last item f =
  printer#append prefix;
  printer#append (if last then l else t);
  f item (prefix ^ if last then "  " else bar)
;;

let render_hashmap ?(prefix = "") printer map f =
  render_children printer map f ?prefix:(Some prefix)
;;

class printer =
  object
    val mutable buf = String.empty
    method append s = buf <- buf ^ s
    method print = print_endline buf
  end
