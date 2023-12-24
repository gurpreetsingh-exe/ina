class decoder buf extmod_id =
  object (self)
    val buf = buf
    val extmod_id : int = extmod_id
    val mutable pos = 0
    method extmod_id = extmod_id

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

let decode_vec dec vec f =
  let size = dec#read_usize in
  for _ = 0 to size - 1 do
    vec#push (f dec)
  done
;;

let decode_hashmap dec map kf vf =
  let size = dec#read_usize in
  for _ = 0 to size - 1 do
    let k = kf dec in
    let v = vf dec in
    map#insert' k v
  done
;;
