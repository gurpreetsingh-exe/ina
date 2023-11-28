open Utils.Panic

module type UnifyValue = sig
  type v
  type e

  val unify_values : v -> v -> (v, e) result
end

module type UnifyKey = sig
  type k
  type v
  type e

  include UnifyValue with type v := v and type e := e

  val index : k -> int
  val from_index : int -> k
  val tag : unit -> string
  val order_roots : k -> v -> k -> v -> (k * k) option
  val display_key : k -> string
  val display_value : v -> string
end

module VarValue (K : UnifyKey) = struct
  type t = {
      mutable parent: K.k
    ; mutable value: K.v
    ; mutable rank: int
  }

  let create parent value = { parent; value; rank = 0 }
  let redirect value too = value.parent <- too

  let display v =
    Printf.sprintf
      "{ parent = %s, value = %s, rank = %d }"
      (K.display_key v.parent)
      (K.display_value v.value)
      v.rank
  ;;

  let root value rank valuee =
    value.rank <- rank;
    value.value <- valuee
  ;;
end

let ( let* ) r f = match r with Ok v -> f v | Error e -> Error e

module Unification_table (K : UnifyKey) = struct
  module VarValue = VarValue (K)

  type t = { mutable values: VarValue.t array }

  let create () = { values = [||] }

  let new_key ut value =
    let len = Array.length ut.values in
    let key = K.from_index len in
    ut.values <- Array.append ut.values [|VarValue.create key value|];
    dbg "%s: created new key: %s\n" (K.tag ()) (K.display_key key);
    key
  ;;

  let value ut key =
    let index = K.index key in
    ut.values.(index)
  ;;

  let rec find ut key =
    let v = value ut key in
    if v.parent = key
    then key
    else
      let parent = find ut v.parent in
      if parent <> v.parent then v.parent <- parent;
      parent
  ;;

  let update_value ut key f =
    let v = value ut key in
    f v;
    dbg
      "updated variable %s to %s\n"
      (K.display_key key)
      (VarValue.display (value ut key))
  ;;

  let redirect_root ut new_rank old_root_key new_root_key new_value =
    update_value ut old_root_key (fun v -> VarValue.redirect v new_root_key);
    update_value ut new_root_key (fun v ->
        VarValue.root v new_rank new_value)
  ;;

  let unify_roots ut key0 key1 new_value =
    dbg
      "unify_roots(key0 = %s, key1 = %s)\n"
      (K.display_key key0)
      (K.display_key key1);
    let rank0 = (value ut key0).rank in
    let rank1 = (value ut key1).rank in
    match
      K.order_roots key0 (value ut key0).value key1 (value ut key1).value
    with
    | Some (new_root, redirect) ->
        let new_rank =
          if new_root = key0
          then if rank0 > rank1 then rank0 else rank1 + 1
          else if rank1 > rank0
          then rank1
          else rank0 + 1
        in
        redirect_root ut new_rank redirect new_root new_value
    | None ->
        if rank0 > rank1
        then redirect_root ut rank0 key1 key0 new_value
        else if rank0 < rank1
        then redirect_root ut rank1 key0 key1 new_value
        else redirect_root ut (rank0 + 1) key0 key1 new_value
  ;;

  let unify_var_var ut t0 t1 =
    let root_t0 = find ut t0 in
    let root_t1 = find ut t1 in
    if root_t0 = root_t1
    then Ok ()
    else
      let* combined =
        K.unify_values (value ut root_t0).value (value ut root_t1).value
      in
      Ok (unify_roots ut root_t0 root_t1 combined)
  ;;

  let unify_var_value ut t0 v =
    dbg
      "unify_var_value(key = %s, value = %s)\n"
      (K.display_key t0)
      (K.display_value v);
    let root_t0 = find ut t0 in
    let* value = K.unify_values (value ut root_t0).value v in
    update_value ut root_t0 (fun k -> k.value <- value);
    Ok ()
  ;;

  let rec get_root_key ut k =
    let v = value ut k in
    if v.parent = k
    then k
    else
      let redirect = v.parent in
      let root_key = get_root_key ut redirect in
      if root_key <> redirect
      then update_value ut k (fun v -> v.parent <- root_key);
      root_key
  ;;

  let probe_value ut k =
    let k = get_root_key ut k in
    (value ut k).value
  ;;
end
