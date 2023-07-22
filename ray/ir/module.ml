type t = { items : Func.t list }

let render modulee =
  List.iter (fun f -> print_endline (Func.render f)) modulee.items

let dot_graph_func name bbs out_dir =
  let strip_tab i =
    let len = String.length i in
    String.sub i 4 (len - 4)
  in
  let bb_buf =
    String.concat ""
      (List.map
         (fun (bb : Inst.basic_block) ->
           Printf.sprintf "    bb%d [label = \"%s\" xlabel = \"bb%d\"];\n"
             bb.bid
             (String.concat ""
                (List.map
                   (fun inst -> strip_tab (Inst.render_inst inst) ^ "\\l")
                   bb.insts))
             bb.bid)
         bbs)
  in
  let out_name = out_dir ^ Filename.dir_sep ^ name in
  let out = open_out (out_name ^ ".dot") in
  "digraph " ^ name ^ " {\n" ^ "    label = \"" ^ name ^ "\";\n"
  ^ "    node [shape = box fontname = monospace fontsize = 12];\n" ^ bb_buf
  ^ String.concat ""
      (List.map
         (fun (bb : Inst.basic_block) ->
           String.concat ""
             (List.map
                (fun (succ : Inst.basic_block) ->
                  Printf.sprintf "    bb%d -> bb%d;\n" bb.bid succ.bid)
                bb.succ))
         bbs)
  ^ "}\n"
  |> output_string out

let dot_graph modulee =
  (try Sys.mkdir ".dots" 0o775 with Sys_error _ -> ());
  List.iter
    (fun f ->
      match f with
      | Func.Decl _ -> ()
      | Def { def_ty; basic_blocks = { bbs } } ->
          dot_graph_func def_ty.name bbs ".dots")
    modulee.items
