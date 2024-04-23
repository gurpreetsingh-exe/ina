open Ast
module Decision = Middle.Decision
open Middle.Decision
open Middle.Ty
open Middle.Def_id
open Middle.Ctx
open Structures.Vec
open Structures.Hashmap
open Format

(* Implementation based on
   https://github.com/yorickpeterse/pattern-matching-in-rust *)

type column = {
    variable: variable
  ; pattern: pat
}

type row = {
    columns: column vec
  ; body: body
}

type diagnostics = {
    mutable missing: bool
  ; reachable: int vec
}

type match' = {
    tree: Decision.t
  ; diag: diagnostics
}

type term = {
    variable: variable
  ; name: string
  ; args: variable vec
}

type compiler = {
    mutable variable_id: int
  ; diag: diagnostics
  ; tcx: tcx
}

let rec compile_rows compiler rows =
  let tcx = compiler.tcx in
  match rows#empty with
  | true ->
      compiler.diag.missing <- true;
      Failure
  | false ->
      rows#iter (move_variable_patterns tcx);
      if rows#first
         |> Option.map (fun row -> row.columns#empty)
         |> Option.value ~default:false
      then (
        let row = rows#pop_front in
        compiler.diag.reachable#push row.body.index;
        Success row.body)
      else
        let bv = branch_var rows in
        (match !(bv.ty) with
         | Int _ ->
             let cases, fallback = compile_int_cases compiler rows bv in
             Switch (bv, cases, Some fallback)
         | Bool ->
             let cases = new vec in
             cases#push (True, new vec, new vec);
             cases#push (False, new vec, new vec);
             Switch
               (bv, compile_constructor_cases compiler rows bv cases, None)
         | Adt _ ->
             let variants = tcx#variants bv.ty |> Option.get in
             let cases =
               map variants (fun (Variant { fields; index; _ }) ->
                   let args = map fields (fun (Field { ty; _ }) -> ty) in
                   ( Decision.Cons (bv.ty, index)
                   , new_variables compiler args
                   , new vec ))
             in
             Switch
               (bv, compile_constructor_cases compiler rows bv cases, None)
         | _ ->
             print_endline @@ tcx#render_ty bv.ty;
             assert false)

and new_variables compiler tys = map tys (new_variable compiler)

and new_variable compiler ty =
  let var = { ty; index = compiler.variable_id } in
  compiler.variable_id <- compiler.variable_id + 1;
  var

and compile_int_cases compiler rows bv =
  let tested = new hashmap in
  let raw_cases = new vec in
  let fallback_rows = new vec in
  rows#iter (fun row ->
      match remove_column row bv with
      | Some col ->
          let key, cons =
            match col.pattern with
            | PInt v -> (v, v), Decision.Int v
            | _ -> assert false
          in
          (match tested#get key with
           | Some idx ->
               let _, _, rows' = raw_cases#get idx in
               rows'#push row
           | None ->
               let rows' = new vec in
               rows'#push row;
               assert (tested#insert key raw_cases#len = None);
               raw_cases#push (cons, new vec, rows'))
      | None -> fallback_rows#push row);
  raw_cases#iter (fun (_, _, rows') -> rows'#append fallback_rows);
  let cases =
    map raw_cases (fun (cons, vars, rows) ->
        { cons; args = vars; body = compile_rows compiler rows })
  in
  cases, compile_rows compiler fallback_rows

and compile_constructor_cases compiler rows bv cases =
  let tcx = compiler.tcx in
  let g row args idx =
    let case = cases#get idx in
    let _, vars, rows' = case in
    iter2 vars args (fun variable pattern ->
        row.columns#push { variable; pattern });
    rows'#push { columns = row.columns; body = row.body }
  in
  let f row args id =
    let idx =
      match tcx#res_map#unsafe_get id with
      | Def (did, Cons) -> tcx#variant_index did
      | _ -> assert false
    in
    g row args idx
  in
  rows#iter (fun row ->
      match remove_column row bv with
      | Some col ->
          (match col.pattern with
           | PCons (path, args) -> f row args path.path_id
           | PPath path -> f row (new vec) path.path_id
           | PIdent (_, _, id)
             when tcx#get_local (local_def_id id) |> Option.is_none ->
               f row (new vec) id
           | PBool true -> g row (new vec) 0
           | PBool false -> g row (new vec) 1
           | PInt _ | PIdent _ | PWild -> ())
      | None -> cases#iter (fun (_, _, rows) -> rows#push row));
  map cases (fun (cons, vars, rows) ->
      { cons; args = vars; body = compile_rows compiler rows })

and remove_column row bv =
  let idx = ref @@ -1 in
  find
    (fun (col : column) ->
      incr idx;
      if col.variable.index = bv.index then Some !idx else None)
    row.columns
  |> Option.map (fun i -> row.columns#remove i)

and branch_var rows =
  let counts = new hashmap in
  rows#iter (fun row ->
      row.columns#iter (fun col ->
          counts#map ~default:0 col.variable (fun i -> i + 1)));
  let vars = map (rows#get 0).columns (fun col -> col.variable) in
  let n = ref @@ -1 in
  let var = ref None in
  vars#iter (fun v ->
      let n' = counts#unsafe_get v in
      if n' > !n
      then (
        n := n';
        var := Some v));
  !var |> Option.get

and move_variable_patterns tcx row =
  row.columns#replace
  @@ (row.columns#filter (fun col ->
          match col.pattern with
          | PIdent (_, _, id) ->
              let did = local_def_id id in
              if tcx#get_local did |> Option.is_some
              then (
                assert (row.body.bindings#insert did col.variable = None);
                false)
              else true
          | PWild -> false
          | _ -> true))
       #inner

and add_missing_patterns compiler decision terms missing =
  let tcx = compiler.tcx in
  match decision with
  | Success _ -> ()
  | Failure ->
      let mapping = Hashtbl.create 0 in
      terms#iteri (fun i term -> Hashtbl.add mapping term.variable i);
      let name =
        terms#first
        |> Option.map (fun term -> pattern_name term terms mapping)
        |> Option.value ~default:"_"
      in
      Hashtbl.add missing name ()
  | Switch (variable, cases, fallback) ->
      cases#iter (fun case ->
          (match case.cons with
           | Cons (ty, idx) ->
               let (Variant variant) =
                 (tcx#variants ty |> Option.get)#get idx
               in
               let s, _ = tcx#into_segments variant.def_id in
               let name = List.rev s |> List.hd in
               terms#push { variable; name; args = case.args }
           | Int _ -> terms#push { variable; name = "_"; args = new vec }
           | True -> terms#push { variable; name = "true"; args = new vec }
           | False -> terms#push { variable; name = "false"; args = new vec });
          add_missing_patterns compiler case.body terms missing;
          ignore @@ terms#pop);
      Option.iter
        (fun decision ->
          add_missing_patterns compiler decision terms missing)
        fallback

and pattern_name term terms mapping =
  if term.args#empty
  then term.name
  else
    let args =
      term.args#join ", " (fun arg ->
          Hashtbl.find_opt mapping arg
          |> Option.map (fun idx ->
                 pattern_name (terms#get idx) terms mapping)
          |> Option.value ~default:"_")
    in
    sprintf "%s(%s)" term.name args
;;

let missing_patterns compiler decision =
  let missing = Hashtbl.create 0 in
  add_missing_patterns compiler decision (new vec) missing;
  Hashtbl.to_seq_keys missing
  |> List.of_seq
  |> List.sort String.compare
  |> List.map (fun s -> "`" ^ s ^ "`")
  |> List.rev
  |> function
  | [] -> assert false
  | name :: [] -> "", name
  | name :: missing ->
      "s", (missing |> List.rev |> String.concat ", ") ^ " and " ^ name
;;

let go (tcx : tcx) scrutinee_ty arms scrutinee_span =
  let compiler =
    { diag = { missing = false; reachable = new vec }; variable_id = 0; tcx }
  in
  let variable = new_variable compiler scrutinee_ty in
  let rows =
    mapi arms (fun index arm ->
        let columns = new vec in
        columns#push { variable; pattern = arm.pat };
        let body = { bindings = new hashmap; index } in
        { columns; body })
  in
  let decision = compile_rows compiler rows in
  let armidx = mapi arms (fun i _ -> i) in
  let unreachable =
    armidx#filter (fun v -> not @@ compiler.diag.reachable#mem v)
  in
  unreachable#iter (fun i ->
      let arm = arms#get i in
      let msg = "unreachable pattern" in
      Errors.Diagnostic.(
        create msg ~level:Warn ~labels:[Label.primary msg arm.patspan])
      |> tcx#emit);
  (if compiler.diag.missing
   then
     let s, missing = missing_patterns compiler decision in
     Errors.Diagnostic.(
       create
         "non-exhaustive patterns"
         ~labels:
           [
             Label.primary
               (sprintf "pattern%s %s not covered" s missing)
               scrutinee_span
           ])
     |> tcx#emit);
  decision
;;

let check_let (tcx : tcx) ty pattern span =
  let compiler =
    { diag = { missing = false; reachable = new vec }; variable_id = 0; tcx }
  in
  let variable = new_variable compiler ty in
  let rows = new vec in
  let columns = new vec in
  columns#push { variable; pattern };
  let body = { bindings = new hashmap; index = 0 } in
  rows#push { columns; body };
  let decision = compile_rows compiler rows in
  (if compiler.diag.missing
   then
     let s, missing = missing_patterns compiler decision in
     Errors.Diagnostic.(
       create
         "refutable pattern in let binding"
         ~labels:
           [
             Label.primary (sprintf "pattern%s %s not covered" s missing) span
           ])
     |> tcx#emit);
  decision
;;
