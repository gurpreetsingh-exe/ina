open Ty
open Printf

type ('a, 'b) var_value = {
    mutable parent: 'a
  ; mutable value: 'b
  ; mutable rank: int
}

let var_value_create key value = { parent = key; value; rank = 0 }

type ('a, 'b) t = { mutable values: ('a, 'b) var_value array }

let new_key (ut : ('a, 'b) t) (value : 'b) : 'a =
  let len = Array.length ut.values in
  let key = { index = len } in
  ut.values <- Array.append ut.values [|var_value_create key value|];
  key
;;

let value (ut : ('a, 'b) t) (key : 'a) =
  let index = key.index in
  ut.values.(index)
;;

let rec find (ut : ('a, 'b) t) (key : 'a) : 'a =
  let v = value ut key in
  if v.parent = key
  then key
  else
    let parent = find ut v.parent in
    if parent <> v.parent then v.parent <- parent;
    parent
;;

let unify_values (v0 : 'a) (v1 : 'a) : ('a, 'a * 'a) result =
  if v0 = v1 then Ok v0 else Error (v0, v1)
;;

let update_value ut key f =
  let value = value ut key in
  f value
;;

let redirect value too = value.parent <- too

let root value rank valuee =
  value.rank <- rank;
  value.value <- valuee
;;

let redirect_root ut new_rank old_root_key new_root_key new_value =
  update_value ut old_root_key (fun v -> redirect v new_root_key);
  update_value ut new_root_key (fun v -> root v new_rank new_value)
;;

let unify_roots ut key0 key1 new_value =
  let rank0 = (value ut key0).rank in
  let rank1 = (value ut key1).rank in
  if rank0 > rank1
  then redirect_root ut rank0 key1 key0 new_value
  else if rank0 < rank1
  then redirect_root ut rank1 key0 key1 new_value
  else redirect_root ut (rank1 + 1) key0 key1 new_value
;;

let unify_var_var ut t0 t1 =
  let root_t0 = find ut t0 in
  let root_t1 = find ut t1 in
  if root_t0 = root_t1
  then ()
  else
    let combined =
      Result.get_ok
        (unify_values (value ut root_t0).value (value ut root_t1).value)
    in
    unify_roots ut root_t0 root_t1 combined
;;

let unify_var_value ut (t0 : 'a) (v : 'b) =
  let root_t0 = find ut t0 in
  let value = Result.get_ok (unify_values (value ut root_t0).value v) in
  update_value ut root_t0 (fun k -> k.value <- value)
;;

let display ut f =
  Array.iter
    (fun v ->
      match v with
      | { parent = { index }; value; _ } ->
          printf "?%d: %s\n" index (f value))
    ut.values
;;
