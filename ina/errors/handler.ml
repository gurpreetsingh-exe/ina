open Source.Source_map
open Emitter
open Diagnostic

class handler source_map ui_testing =
  object
    val sm : source_map option = source_map
    val emitter : emitter = new emitter source_map ui_testing
    val mutable err_count = 0
    method err_count = err_count
    method span_err span msg = mk_err msg span

    method emit_diagnostic diag =
      err_count <- err_count + 1;
      emitter#emit_diagnostic diag
  end
