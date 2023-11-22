open Structures.Vec

type t = { items: Func.t vec }

let render tcx modulee =
  modulee.items#iter (fun f -> print_endline (Func.render tcx f))
;;

let dot_graph_func name bbs out_dir =
  let strip_tab i =
    let len = String.length i in
    String.sub i 4 (len - 4)
  in
  let bb_buf =
    bbs#join "" (fun (bb : Inst.basic_block) ->
        Printf.sprintf
          "    bb%d [label = \"%s\" xlabel = \"bb%d\"];\n"
          bb.bid
          (bb.insts#join "" (fun inst ->
               strip_tab (Inst.render_inst inst) ^ "\\l"))
          bb.bid)
  in
  let out_name = out_dir ^ Filename.dir_sep ^ name in
  let out = open_out (out_name ^ ".dot") in
  "digraph "
  ^ name
  ^ " {\n"
  ^ "    label = \""
  ^ name
  ^ "\";\n"
  ^ "    node [shape = box fontname = monospace fontsize = 12];\n"
  ^ bb_buf
  ^ bbs#join "" (fun (bb : Inst.basic_block) ->
        bb.succ#join "" (fun (succ : Inst.basic_block) ->
            Printf.sprintf "    bb%d -> bb%d;\n" bb.bid succ.bid))
  ^ "}\n"
  |> output_string out
;;

let dot_graph modulee =
  (try Sys.mkdir ".dots" 0o775 with Sys_error _ -> ());
  modulee.items#iter (fun Func.{ ty; def_id; basic_blocks = { bbs }; _ } ->
      dot_graph_func (Middle.Def_id.print_def_id def_id) bbs ".dots")
;;
