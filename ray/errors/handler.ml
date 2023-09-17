type t = {
  emitter : Emitter.t;
  mutable err_count : int;
}

let create sm ui_testing =
  { emitter = { sm = Some sm; ui_testing }; err_count = 0 }

let early () = { emitter = { sm = None; ui_testing = false }; err_count = 0 }

let emit_err handle err =
  handle.err_count <- handle.err_count + 1;
  Emitter.emit handle.emitter err

let emit_small_err handle msg =
  emit_err handle
    Diagnostic.
      {
        message = msg;
        level = Err;
        span = { primary_spans = []; labels = [] };
        children = [];
        sugg = [];
        loc = Diagnostic.loc __POS__;
      }
