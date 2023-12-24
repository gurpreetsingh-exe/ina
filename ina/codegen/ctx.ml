open Middle.Ctx
open Utils.Path
open Structures.Vec
open Structures.Hashmap

module type CodegenBackend = sig
  type cx

  val out : string ref
  val create : tcx -> Ir.Module.t -> cx
  val gen : cx -> unit
  val emit : cx -> string -> unit
  val mangle : cx -> Middle.Def_id.def_id -> string
end

module MakeCodegenBackend (T : CodegenBackend) = struct
  type cx = T.cx

  let create tcx irmdl = T.create tcx irmdl
  let gen cx = T.gen cx
  let emit cx output = T.emit cx output
end

let codegen (tcx : tcx) mdl =
  match tcx#sess.options.backend with
  | C ->
      let module Backend = MakeCodegenBackend (C) in
      let compiler = "clang" in
      let cx = Backend.create tcx mdl in
      Backend.gen cx;
      let output = tcx#sess.options.output in
      let input = output ^ ".c" in
      Backend.emit cx input;
      let open Printf in
      let command =
        sprintf
          "%s -ggdb -std=c17 -w %s"
          compiler
          (match tcx#sess.options.opt_level with
           | Default -> "-O0"
           | Agressive -> "-O3")
      in
      let units = new hashmap in
      tcx#extern_mods#iter (fun u -> units#insert' u ());
      let objs = ref "" in
      units#iter (fun u _ -> objs := sprintf "%s %s" !objs u);
      let objs = !objs in
      let command =
        match tcx#sess.options.output_type with
        | Exe ->
            let cmd = sprintf "%s -c %s -o %s.o" command input output in
            assert (Sys.command cmd = 0);
            sprintf "%s -o %s %s.o %s" command output output objs
        | Object -> sprintf "%s -c %s -o %s.o" command input output
        | Asm -> sprintf "%s -S -masm=intel %s" command input
        | Unit ->
            sprintf
              "%s -c %s -o %s.o"
              command
              input
              (add_suffix output "lib")
      in
      if Sys.command command <> 0 then eprintf "command failed\n"
      (* assert (Sys.command (sprintf "rm -f %s" input) = 0) *)
  | Llvm -> assert false
;;
