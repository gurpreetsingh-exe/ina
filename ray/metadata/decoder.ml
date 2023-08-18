type t = {
  buf : bytes;
  mutable pos : int;
}

let create buf = { buf; pos = 0 }

let read_usize dec : int =
  let n = Bytes.get_int64_be dec.buf dec.pos |> Int64.to_int in
  dec.pos <- dec.pos + 8;
  n

let read_u8 dec : int =
  let n = Bytes.get_uint8 dec.buf dec.pos in
  dec.pos <- dec.pos + 1;
  n

let read_u32 dec : int =
  let n = Bytes.get_int32_be dec.buf dec.pos |> Int32.to_int in
  dec.pos <- dec.pos + 4;
  n

let read_str dec : string =
  let len = read_usize dec in
  let str = Bytes.sub_string dec.buf dec.pos len in
  dec.pos <- dec.pos + len;
  str
