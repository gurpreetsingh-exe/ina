open Printf

module Array = struct
  include Array

  let last arr : 'a option =
    match length arr with 0 -> None | i -> Some arr.(i - 1)

  let partition_point arr f : int =
    let left = ref 0 in
    let size = ref @@ length arr in
    let right = ref @@ length arr in
    while !left < !right do
      let half = !size / 2 in
      let middle = !left + half in
      if f arr.(middle) then left := middle + 1 else right := middle;
      size := !right - !left
    done;
    !left
end

module File = struct
  type id = SourceFileId of int

  type lines = int array

  type t = {
    name : string;
    src : string;
    lines : lines;
    mutable start_pos : int;
  }

  let relative_pos f pos = pos - f.start_pos

  let lookup_line_src f line =
    let s = f.lines.(line - 1) in
    let e = f.lines.(line) in
    String.sub f.src s (e - s)

  let lookup_line f pos =
    match Array.partition_point f.lines (fun x -> x <= pos) with
    | 0 -> None
    | x -> Some (x - 1)

  let lookup_file_pos f pos =
    let pos = relative_pos f pos in
    match lookup_line f pos with
    | Some l ->
        let linepos = f.lines.(l) in
        (l + 1, pos - linepos)
    | None -> (0, pos)

  let create name src =
    let f (s : string) =
      let arr = ref [||] in
      let i = ref 0 in
      while !i < String.length s do
        (match s.[!i] with
        | '\n' -> arr := Array.append !arr [|!i + 1|]
        | _ -> ());
        incr i
      done;
      !arr
    in
    { name; src; lines = f src; start_pos = 0 }

  let end_pos file = String.length file.src + file.start_pos

  let display file =
    printf "{ %s; [%s] }" file.name
      (String.concat ", "
         (Array.map (fun i -> string_of_int i) file.lines |> Array.to_list))
end

type t = {
  mutable source_files : File.t array;
  file_id_to_file : (File.id, File.t) Hashtbl.t;
}

let create () = { source_files = [||]; file_id_to_file = Hashtbl.create 0 }

let file_from_id map id = Hashtbl.find_opt map.file_id_to_file id

let lookup_file_index map pos =
  Array.partition_point map.source_files (fun x -> x.start_pos <= pos) - 1

let lookup_file map pos =
  let i = lookup_file_index map pos in
  map.source_files.(i)

let add_file map name src : File.t =
  let fileid = File.SourceFileId (Hashtbl.hash name) in
  match file_from_id map fileid with
  | Some f -> f
  | None ->
      let file = File.create name src in
      let pos =
        match Array.last map.source_files with
        | Some f -> File.end_pos f + 1
        | None -> 0
      in
      file.start_pos <- pos;
      map.source_files <- Array.append map.source_files [|file|];
      Hashtbl.add map.file_id_to_file fileid file;
      file

let load_file map name =
  if String.length name = 0 then (
    fprintf stderr "error: no input files provided\n";
    exit 1)
  else if Sys.file_exists name then (
    let ic = open_in name in
    let s = really_input_string ic (in_channel_length ic) in
    if not (String.is_valid_utf_8 s) then (
      fprintf stderr
        "error: cannot process `%s`, file is not valid UTF-8 string\n" name;
      exit 1);
    if Filename.extension name = "" then (
      fprintf stderr
        "error: input file `%s` will be overwritten by the executable\n" name;
      exit 1);
    close_in ic;
    add_file map name s)
  else (
    fprintf stderr "error: cannot open `%s`, file doesn't exist\n" name;
    exit 1)

let span_to_string map pos =
  let file = lookup_file map pos in
  let line, col = File.lookup_file_pos file pos in
  sprintf "%s:%d:%d" file.name (line + 1) (col + 1)
