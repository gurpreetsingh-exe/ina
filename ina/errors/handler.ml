open Source.Source_map
open Emitter
open Diagnostic

class handler source_map ui_testing =
  object
    val sm : source_map option = source_map
    val emitter : emitter = new emitter source_map ui_testing
    method span_err span msg = mk_err msg span
    method emit_diagnostic diag = emitter#emit_diagnostic diag
  end
