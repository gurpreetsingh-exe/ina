open Printf
open Structures.Vec

type file_id = SourceFileId of int

class file (name : string) (src : string) =
  let get_lines =
    let lines = new vec in
    let i = ref 0 in
    while !i < String.length src do
      (match src.[!i] with '\n' -> lines#push (!i + 1) | _ -> ());
      incr i
    done;
    lines
  in
  object (self)
    val name = name
    val src = src
    val lines = get_lines
    val mutable start_pos = 0

    (* getters *)
    method name = name
    method src = src
    method start_pos = start_pos

    (* setters*)
    method set_start_pos pos = start_pos <- pos

    (* public methods *)
    method relative_pos pos = pos - start_pos
    method end_pos = String.length src + start_pos

    method lookup_line_src line =
      let s = lines#get (line - 1) in
      let e = lines#get line in
      String.sub src s (e - s)

    method lookup_line pos =
      match lines#partition_point (fun x -> x <= pos) with
      | 0 -> None
      | x -> Some (x - 1)

    method lookup_file_pos pos =
      let pos = self#relative_pos pos in
      match self#lookup_line pos with
      | Some l ->
          let linepos = lines#get l in
          l + 1, pos - linepos
      | None -> 0, pos
  end

class source_map =
  object (self)
    val source_files : file vec = new vec
    val file_id_to_file : (file_id, file) Hashtbl.t = Hashtbl.create 0

    method add_file name src =
      let fileid = SourceFileId (Hashtbl.hash name) in
      match self#file_from_id fileid with
      | Some f -> f
      | None ->
          let file = new file name src in
          let pos =
            match source_files#last with
            | Some f -> f#end_pos + 1
            | None -> 0
          in
          file#set_start_pos pos;
          source_files#push file;
          Hashtbl.add file_id_to_file fileid file;
          file

    method load_file name =
      if String.length name = 0
      then (
        fprintf stderr "error: no input files provided\n";
        exit 1)
      else if Sys.file_exists name
      then (
        let ic = open_in name in
        let s = really_input_string ic (in_channel_length ic) in
        if not (String.is_valid_utf_8 s)
        then (
          fprintf
            stderr
            "error: cannot process `%s`, file is not valid UTF-8 string\n"
            name;
          exit 1);
        if Filename.extension name = ""
        then (
          fprintf
            stderr
            "error: input file `%s` will be overwritten by the executable\n"
            name;
          exit 1);
        close_in ic;
        self#add_file name s)
      else (
        fprintf stderr "error: cannot open `%s`, file doesn't exist\n" name;
        exit 1)

    method lookup_file_index pos =
      source_files#partition_point (fun x -> x#start_pos <= pos) - 1

    method file_from_id id = Hashtbl.find_opt file_id_to_file id

    method lookup_file pos =
      let i = self#lookup_file_index pos in
      source_files#get i

    method span_to_string pos =
      let file = self#lookup_file pos in
      let line, col = file#lookup_file_pos pos in
      sprintf "%s:%d:%d" file#name (line + 1) (col + 1)
  end
