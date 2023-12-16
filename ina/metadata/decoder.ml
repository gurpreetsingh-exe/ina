class decoder buf unit_id =
  object (self)
    val buf = buf
    val unit_id : int = unit_id
    val mutable pos = 0
    method unit_id = unit_id

    method read_usize : int =
      let n = Bytes.get_int64_be buf pos |> Int64.to_int in
      pos <- pos + 8;
      n

    method read_u8 : int =
      let n = Bytes.get_uint8 buf pos in
      pos <- pos + 1;
      n

    method read_bool : bool =
      match self#read_u8 with 0 -> false | 1 -> true | _ -> assert false

    method read_u32 : int =
      let n = Bytes.get_int32_be buf pos |> Int32.to_int in
      pos <- pos + 4;
      n

    method read_str : string =
      let len = self#read_usize in
      let str = Bytes.sub_string buf pos len in
      pos <- pos + len;
      str
  end
