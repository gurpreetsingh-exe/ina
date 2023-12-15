let join segments =
  List.filter (fun path -> path <> Filename.current_dir_name) segments
  |> String.concat Filename.dir_sep
;;

let with_ext path ext = Filename.remove_extension path ^ ext

let add_suffix path suff =
  let dir = Filename.dirname path
  and name = Filename.basename path in
  join [dir; suff ^ name]
;;
