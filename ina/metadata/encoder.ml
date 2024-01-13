class encoder =
  object (self)
    val buf : Buffer.t = Buffer.create 0
    val mutable mod_id : int = 0
    val mutable mod_name : string = "<anon>"
    method mod_id = mod_id

    method set_mod_name name =
      mod_name <- name;
      mod_id <- Hashtbl.hash name

    method emit_mod_id = self#emit_usize mod_id

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

let encode_vec enc vec f =
  enc#emit_usize vec#len;
  vec#iter (fun v -> f enc v)
;;

let encode_hashmap enc map kf vf =
  enc#emit_usize map#len;
  map#iter (fun k v ->
      kf enc k;
      vf enc v)
;;

module type Encodable = sig
  type t

  val encode : encoder -> t -> unit
end
