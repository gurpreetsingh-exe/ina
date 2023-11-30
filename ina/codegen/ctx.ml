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
      let cx = Backend.create tcx mdl in
      Backend.gen cx;
      let output = tcx#sess.options.output ^ ".c" in
      Backend.emit cx output;
      let command = Printf.sprintf "gcc %s" output in
      if Sys.command command <> 0
      then Printf.fprintf stderr "cannot emit executable\n"
  | Llvm -> assert false
;;
