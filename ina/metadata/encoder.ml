type t = { buf: Buffer.t }

let create _ = { buf = Buffer.create 0 }
let data enc = String.of_bytes @@ Buffer.to_bytes enc.buf
let emit_usize enc n = Buffer.add_int64_be enc.buf @@ Int64.of_int n
let emit_u8 enc n = Buffer.add_uint8 enc.buf n
let emit_bool enc b = emit_u8 enc (if b then 1 else 0)
let emit_u32 enc n = Buffer.add_int32_be enc.buf @@ Int32.of_int n

let emit_with (enc : t) (n : int64) (f : t -> unit) =
  Buffer.add_int64_be enc.buf n;
  f enc
;;

let emit_str enc str =
  emit_with
    enc
    (String.length str |> Int64.of_int)
    (fun e -> Buffer.add_string e.buf str)
;;
