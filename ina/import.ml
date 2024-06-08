let ( $ ) f g h = f (g h)

external unsafe_blit
  :  'a array
  -> int
  -> 'a array
  -> int
  -> int
  -> unit
  = "caml_array_blit"

module Vec = struct
  type 'a t = {
      mutable inner: 'a array
    ; mutable length: int
  }

  let create () = { inner = [||]; length = 0 }
  let get self = Array.sub self.inner 0 self.length

  let bounds_check self index =
    if not (index < self.length) then assert false
  ;;

  (* [%ice "vector out of bounds"] *)
  let empty self = self.length = 0

  let clear self =
    self.inner <- [||];
    self.length <- 0
  ;;

  let pp f fmt self =
    let open Format in
    let arr = get self in
    let go sep x =
      if sep then fprintf fmt ";@ ";
      f fmt x;
      true
    in
    fprintf fmt "@[<2>[|";
    ignore (Array.fold_left go false arr);
    fprintf fmt "@,|]@]"
  ;;

  let show f = Format.asprintf "%a" (pp f)

  let ( .%[] ) self index =
    bounds_check self index;
    Array.unsafe_get self.inner index
  ;;

  let ( .%[]<- ) self index value =
    bounds_check self index;
    Array.unsafe_set self.inner index value
  ;;

  let from_capacity capacity =
    { inner = Array.init capacity (fun _ -> Obj.magic ()); length = 0 }
  ;;

  let of_array a =
    let capacity = Array.length a in
    { inner = a; length = capacity }
  ;;

  let grow self new_capacity =
    let tmp = self.inner in
    self.inner <- Array.init new_capacity (fun _ -> Obj.magic ());
    Array.blit tmp 0 self.inner 0 (Array.length tmp)
  ;;

  let ensure_capacity self =
    if self.length >= Array.length self.inner then grow self (self.length * 2)
  ;;

  let ( << ) self value =
    ensure_capacity self;
    Array.unsafe_set self.inner self.length value;
    self.length <- self.length + 1
  ;;

  let pop self =
    self.length <- self.length - 1;
    self.%[self.length]
  ;;

  let ( <<< ) self other =
    let length_combined = self.length + other.length in
    if length_combined >= Array.length self.inner
    then grow self length_combined;
    unsafe_blit other.inner 0 self.inner self.length other.length;
    self.length <- length_combined
  ;;

  let join self ?(sep = "; ") f =
    let open Format in
    let buf = Buffer.create 0 in
    let fmt = formatter_of_buffer buf in
    let arr = get self in
    let go s x =
      if s then fprintf fmt "%s" sep;
      fprintf fmt "%s" (f x);
      true
    in
    ignore (Array.fold_left go false arr);
    pp_print_flush fmt ();
    Buffer.contents buf
  ;;

  let map f self = { self with inner = Array.map f (get self) }
  let find f self = Array.find_map f (get self)
end

module HashMap = struct
  type ('a, 'b) t = ('a, 'b) Hashtbl.t

  let create = Hashtbl.create
  let ( .%{} ) = Hashtbl.find_opt
  let ( .%{}<- ) = Hashtbl.add
end

let ( << ) = Vec.( << )
let ( <<< ) = Vec.( <<< )
let ( .%[] ) = ( Vec.( .%[] ) )
let ( .%[]<- ) = ( Vec.( .%[]<- ) )
let ( >>| ) a f = Vec.map f a
let ( .%{} ) = ( HashMap.( .%{} ) )
let ( .%{}<- ) = ( HashMap.( .%{}<- ) )
