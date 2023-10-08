open Source.Source_map
open Structures.Vec
open Emitter
open Diagnostic

class handler source_map ui_testing =
  object
    val sm : source_map option = source_map
    val emitter : emitter = new emitter source_map ui_testing

    method span_err span msg =
      let messages = new vec in
      messages#push msg;
      let primary_spans = new vec in
      primary_spans#push span;
      new diagnostic Err messages (new multi_span ~primary_spans ())

    method emit_diagnostic diag = emitter#emit_diagnostic diag
  end
