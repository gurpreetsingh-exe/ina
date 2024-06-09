open Printf
open Structures.Vec

type file_id = SourceFileId of int

module Array = struct
  include Array

  let partition_point f arr =
    let left = ref 0 in
    let size = ref @@ Array.length arr in
    let right = ref @@ Array.length arr in
    while !left < !right do
      let half = !size / 2 in
      let middle = !left + half in
      if f arr.(middle) then left := middle + 1 else right := middle;
      size := !right - !left
    done;
    !left
  ;;
end

class file (name : string) (src : string) =
  let get_lines =
    let line_len = ref 0 in
    let lines = ref @@ Array.make 8 0 in
    let i = ref 0 in
    let len = String.length src in
    while !i < len do
      if src.[!i] = '\n'
      then (
        if !line_len >= Array.length !lines
        then (
          let tmp = !lines in
          lines := Array.make (!line_len * 2) 0;
          Array.blit tmp 0 !lines 0 !line_len);
        Array.unsafe_set !lines !line_len (!i + 1);
        incr line_len);
      incr i
    done;
    Array.sub !lines 0 !line_len
  in
  object (self)
    val name = name
    val src = src
    val lines = get_lines
    val mutable start_pos = 0
    method name = name
    method src = src
    method start_pos = start_pos
    method set_start_pos pos = start_pos <- pos
    method relative_pos pos = pos - start_pos
    method end_pos = String.length src + start_pos

    method lookup_line_src line =
      let s, e =
        if line = 0
        then
          let e = lines.(line) in
          0, e
        else
          let s = lines.(line - 1) in
          let e = lines.(line) in
          s, e
      in
      String.sub src s (e - s - 1)

    method lookup_line pos =
      match Array.partition_point (fun x -> x <= pos) lines with
      | 0 -> None
      | x -> Some (x - 1)

    method lookup_file_pos pos =
      let pos = self#relative_pos pos in
      match self#lookup_line pos with
      | Some l ->
          let linepos = lines.(l) in
          l + 1, pos - linepos
      | None -> 0, pos
  end

class source_map =
  object (self)
    val source_files : file vec = new vec
    val file_id_to_file_index : (file_id, int) Hashtbl.t = Hashtbl.create 0

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
          let index = source_files#len in
          source_files#push file;
          Hashtbl.add file_id_to_file_index fileid index;
          file

    method replace_file name src =
      let fileid = SourceFileId (Hashtbl.hash name) in
      match self#file_index_from_id fileid with
      | Some idx ->
          let file = new file name src in
          let pos =
            if idx = 0 then 0 else (source_files#get (idx - 1))#end_pos + 1
          in
          file#set_start_pos pos;
          source_files#set idx file;
          file
      | None -> self#add_file name src

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

    method file_index_from_id id = Hashtbl.find_opt file_id_to_file_index id

    method file_from_id id =
      Hashtbl.find_opt file_id_to_file_index id
      |> Option.map (fun i -> source_files#get i)

    method lookup_file pos =
      let i = self#lookup_file_index pos in
      source_files#get i

    method lookup_line_pos pos =
      let file = self#lookup_file pos in
      file#lookup_file_pos pos

    method is_same_line (span : Span.t) =
      let file = self#lookup_file span.lo in
      file#lookup_line span.lo = file#lookup_line span.hi

    method lookup_line_src pos =
      let file = self#lookup_file pos in
      let line, _ = file#lookup_file_pos pos in
      file#lookup_line_src line

    method span_to_string pos =
      let file = self#lookup_file pos in
      let line, col = file#lookup_file_pos pos in
      sprintf "%s:%d:%d" file#name (line + 1) (col + 1)
  end
