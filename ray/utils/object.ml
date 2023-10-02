(* [%%if os = "Unix"] *)

type raw_section_header = {
  sh_name : int32;
  sh_type : int32;
  sh_flags : int64;
  sh_addr : int64;
  sh_offset : int64;
  sh_size : int64;
  sh_link : int32;
  sh_info : int32;
  sh_addralign : int64;
  sh_entsize : int64;
}

and obj = {
  buf : bytes;
  raw_shdrs : raw_section_header array;
  string_table : bytes;
}
(* [@@deriving show] *)

let find_raw_shdr obj sh_type : raw_section_header option =
  let shdr = ref None in
  for i = 0 to Array.length obj.raw_shdrs - 1 do
    let shdr' = obj.raw_shdrs.(i) in
    if shdr'.sh_type = sh_type then shdr := Some shdr'
  done;
  !shdr

let shdr_size = 64

let rec read_obj n : obj =
  let b = open_in_bin n in
  let len = in_channel_length b in
  let buf = Bytes.create len in
  really_input b buf 0 len;
  let raw_shdrs = read_sections buf in
  let e_shstrndx = Bytes.get_int16_le buf 0x3e in
  let shdr = raw_shdrs.(e_shstrndx) in
  let string_table =
    Bytes.sub buf
      (shdr.sh_offset |> Int64.to_int)
      (shdr.sh_size |> Int64.to_int)
  in
  { buf; raw_shdrs; string_table }

and read_sections buf : raw_section_header array =
  let shoff = Bytes.get_int64_le buf 0x28 |> Int64.to_int in
  let shnum = Bytes.get_int16_le buf 0x3c in
  let shdrs = ref [||] in
  let read_section i =
    let off = shoff + (i * shdr_size) in
    shdrs :=
      Array.append !shdrs
        [|
          {
            sh_name = Bytes.get_int32_le buf off;
            sh_type = Bytes.get_int32_le buf (off + 0x04);
            sh_flags = Bytes.get_int64_le buf (off + 0x08);
            sh_addr = Bytes.get_int64_le buf (off + 0x10);
            sh_offset = Bytes.get_int64_le buf (off + 0x18);
            sh_size = Bytes.get_int64_le buf (off + 0x20);
            sh_link = Bytes.get_int32_le buf (off + 0x28);
            sh_info = Bytes.get_int32_le buf (off + 0x2c);
            sh_addralign = Bytes.get_int64_le buf (off + 0x30);
            sh_entsize = Bytes.get_int64_le buf (off + 0x38);
          };
        |]
  in
  for i = 0 to shnum - 1 do
    read_section i
  done;
  !shdrs

and read_section obj shdr =
  Bytes.sub obj.buf
    (shdr.sh_offset |> Int64.to_int)
    (shdr.sh_size |> Int64.to_int)

and find_section_by_name obj section_name =
  let index =
    Str.search_forward (Str.regexp section_name)
      (String.of_bytes obj.string_table)
      0
    |> Int32.of_int
  in
  let shdr = ref None in
  for i = 0 to Array.length obj.raw_shdrs - 1 do
    let shdr' = obj.raw_shdrs.(i) in
    if shdr'.sh_name = index then shdr := Some shdr'
  done;
  !shdr

and read_section_by_name obj section_name =
  match find_section_by_name obj section_name with
  | Some shdr -> Some (read_section obj shdr)
  | None -> None

(* [%%elif os = "Win32"] *)
(**)
(* let read_obj _ = assert false *)
(**)
(* [%%else] *)
(**)
(* let read_obj _ = assert false *)
(**)
(* [%%endif] *)
