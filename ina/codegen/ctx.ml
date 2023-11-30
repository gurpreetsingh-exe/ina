open Middle.Ctx

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
          "%s -std=c17 %s"
          compiler
          (match tcx#sess.options.opt_level with
           | Default -> "-O0"
           | Agressive -> "-O3")
      in
      let command =
        match tcx#sess.options.output_type with
        | Exe -> sprintf "%s -o %s %s" command output input
        | Object -> sprintf "%s -c %s -o %s" command input (output ^ ".o")
        | Asm -> sprintf "%s -S -masm=intel %s" command input
        | Unit -> ""
      in
      if Sys.command command <> 0 then eprintf "command failed\n";
      assert (Sys.command (sprintf "rm -f %s" input) = 0)
  | Llvm -> assert false
;;
