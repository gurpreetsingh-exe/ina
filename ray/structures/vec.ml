class ['a] vec =
  object (self)
    val mutable inner = [||]
    method push (v : 'a) = inner <- Array.append inner [|v|]
    method get i = Array.unsafe_get inner i
    method set i v = Array.unsafe_set inner i v
    method replace v = inner <- v
    method len = Array.length inner
    method empty = Array.length inner = 0
    method clear = inner <- [||]
    method all f = Array.for_all f inner
    method any f = Array.exists f inner

    method private split i =
      let left = Array.sub inner 0 i
      and right = Array.sub inner (i + 1) (Array.length inner - 1) in
      left, right

    method insert i v =
      let left, right = self#split i in
      inner <- Array.append (Array.append left [|v|]) right

    method resize l v =
      let r = Array.make l v in
      Array.blit inner 0 r 0 (Array.length inner);
      inner <- r

    method first =
      match Array.length inner with 0 -> None | _ -> Some inner.(0)

    method last =
      match Array.length inner with 0 -> None | i -> Some inner.(i - 1)

    method iter f = Array.iter f inner
    method iteri f = Array.iteri f inner

    method join sep f =
      Array.to_list inner |> List.map f |> String.concat sep

    method partition_point f =
      let left = ref 0 in
      let size = ref @@ Array.length inner in
      let right = ref @@ Array.length inner in
      while !left < !right do
        let half = !size / 2 in
        let middle = !left + half in
        if f inner.(middle) then left := middle + 1 else right := middle;
        size := !right - !left
      done;
      !left
  end

let map v (f : 'a -> 'b) =
  let mapped = new vec in
  let l = v#len in
  if l <> 0
  then (
    let r = Array.make l (f (v#get 0)) in
    for i = 1 to l - 1 do
      Array.unsafe_set r i (f @@ v#get i)
    done;
    mapped#replace r);
  mapped
;;

let ( / ) (v : 'a vec) (i : int) = v#get i
