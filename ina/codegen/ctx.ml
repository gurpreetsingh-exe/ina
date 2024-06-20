open Ir
open Middle.Ctx
open Utils.Path
open Structures.Hashmap
open Structures.Vec
open Monomorphize

module type CodegenBackend = sig
  type cx

  val out : string ref
  val create : tcx -> Ir.Module.t -> cx
  val gen : cx -> unit
  val emit : cx -> string -> unit
end

module MakeCodegenBackend (T : CodegenBackend) = struct
  type cx = T.cx

  let create tcx irmdl = T.create tcx irmdl
  let gen cx = T.gen cx
  let emit cx output = T.emit cx output
end

let codegen (tcx : tcx) (mdl : Module.t) =
  let pending_decoders = tcx#decoders in
  let open Inst in
  let items = new vec in
  let cached = InstanceMap.create 0 in
  pending_decoders#iter (fun dec ->
      let mdl = Ir.Module.decode tcx dec in
      items#append mdl.items;
      let c = new hashmap in
      Metadata.Decoder.decode_hashmap
        dec
        c
        (Inst.decode_instance tcx)
        (fun _ -> ());
      c#iter (fun k _ -> InstanceMap.replace cached k ()));
  mdl.items#append items;
  let items = Collect.collect tcx mdl cached in
  let mdl = Ir.Module.{ items } in
  let opt = tcx#sess.options in
  match opt.backend with
  | C ->
      let module Backend = MakeCodegenBackend (C) in
      let compiler = "clang" in
      let cx = Backend.create tcx mdl in
      Backend.gen cx;
      let output =
        if opt.command = Test then opt.output ^ "_test" else opt.output
      in
      let input = output ^ ".c" in
      Backend.emit cx input;
      if tcx#has_errors then exit 1;
      let open Printf in
      let command =
        sprintf
          "%s -ggdb -w %s"
          compiler
          (match tcx#sess.options.opt_level with
           | Default -> "-O0"
           | Agressive -> "-O3")
      in
      let extmods = new hashmap in
      tcx#extern_mods#iter (fun u -> extmods#insert' u ());
      let objs = ref "" in
      extmods#iter (fun u _ -> objs := sprintf "%s %s" !objs u);
      let objs = !objs in
      let command =
        match opt.output_type with
        | Exe ->
            let cmd = sprintf "%s -c %s -o %s.o" command input output in
            assert (Sys.command cmd = 0);
            sprintf "%s -o %s %s.o %s" command output output objs
        | Object -> sprintf "%s -c %s -o %s.o" command input output
        | Asm -> sprintf "%s -S -masm=intel %s" command input
        | ExtMod ->
            sprintf
              "%s -c %s -o %s.o"
              command
              input
              (add_suffix output "lib")
      in
      if Sys.command command <> 0 then eprintf "command failed\n";
      (* assert (Sys.command (sprintf "rm -f %s %s.o" input output) = 0); *)
      if opt.command = Test
      then (
        let code = Sys.command (sprintf "./%s" output) in
        assert (Sys.command (sprintf "rm -f %s" output) = 0);
        exit code)
  | Llvm -> assert false
;;
