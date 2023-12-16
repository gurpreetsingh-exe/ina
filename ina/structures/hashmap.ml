open Printf
open Metadata.Encoder

class ['k, 'v] hashmap =
  object (self)
    val inner : ('k, 'v) Hashtbl.t = Hashtbl.create 0

    method insert k v =
      if self#has k
      then (
        let old_val = self#get k in
        Hashtbl.replace inner k v;
        old_val)
      else (
        Hashtbl.add inner k v;
        None)

    method remove k =
      if self#has k
      then (
        let old_val = self#get k in
        Hashtbl.remove inner k;
        old_val)
      else None

    method insert' k v = if not (self#has k) then Hashtbl.add inner k v
    method get k = Hashtbl.find_opt inner k
    method unsafe_get k = Hashtbl.find inner k
    method has k = Hashtbl.mem inner k
    method len = Hashtbl.length inner
    method iter f = Hashtbl.iter f inner
    method iteri f = Seq.iteri f (Hashtbl.to_seq inner)

    method print ?(depth = 0) key_print value_print =
      let indent = String.make (depth * 2) ' ' in
      let print_pair (key, value) =
        sprintf "%s%s: %s" indent (key_print key) (value_print value)
      in
      Hashtbl.to_seq inner
      |> Array.of_seq
      |> Array.map print_pair
      |> Array.to_list
      |> String.concat "\n"
  end

module EncodableHashmap (K : Encodable) (V : Encodable) : Encodable = struct
  type k = K.t
  type v = V.t
  type t = (k, v) hashmap

  let encode (enc : encoder) (map : t) =
    enc#emit_usize map#len;
    map#iter (fun k v ->
        K.encode enc k;
        V.encode enc v)
  ;;
end
