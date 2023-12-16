class encoder =
  object (self)
    val buf : Buffer.t = Buffer.create 0

    (* method render = String.of_bytes @@ Buffer.to_bytes buf *)
    method render = Buffer.to_bytes buf
    method emit_usize n = Buffer.add_int64_be buf @@ Int64.of_int n
    method emit_u8 n = Buffer.add_uint8 buf n
    method emit_bool b = self#emit_u8 (if b then 1 else 0)
    method emit_u32 n = Buffer.add_int32_be buf @@ Int32.of_int n

    method emit_with n f =
      Buffer.add_int64_be buf n;
      f self

    method emit_str str =
      self#emit_with
        (String.length str |> Int64.of_int)
        (fun _ -> Buffer.add_string buf str)
  end

module type Encodable = sig
  type t

  val encode : encoder -> t -> unit
end
