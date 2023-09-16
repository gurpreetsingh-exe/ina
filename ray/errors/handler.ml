type t = { emitter : Emitter.t }

let create sm ui_testing = { emitter = { sm = Some sm; ui_testing } }

let early () = { emitter = { sm = None; ui_testing = false } }

let emit_err handle err = Emitter.emit handle.emitter err

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
