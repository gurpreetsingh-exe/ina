class ['a] vec =
  object
    val mutable inner = [||]
    method push (v : 'a) = inner <- Array.append inner [|v|]
    method get i = inner.(i)
    method len = Array.length inner

    method last =
      match Array.length inner with 0 -> None | i -> Some inner.(i - 1)

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
