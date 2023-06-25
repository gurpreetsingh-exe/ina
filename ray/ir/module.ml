type t = { items : Func.t list }

let render modulee =
  List.iter (fun f -> print_endline (Func.render f)) modulee.items
