open Printf
open Source
open Structures.Vec

let e = "\x1b[0m"

type level =
  | Note
  | Warn
  | Err
  | Bug

let level_to_color = function
  | Note -> "\x1b[1;36m"
  | Warn -> "\x1b[1;33m"
  | Err | Bug -> "\x1b[1;31m"
;;

let display_level = function
  | Note -> "note"
  | Warn -> "warning"
  | Err -> "error"
  | Bug -> "internal compiler error"
;;

let render_level level =
  sprintf "%s%s%s" (level_to_color level) (display_level level) e
;;

type style =
  | MainHeaderMsg
  | HeaderMsg
  | LineAndColumn
  | LineNumber
  | Quotation
  | UnderlinePrimary
  | UnderlineSecondary
  | LabelPrimary
  | LabelSecondary
  | NoStyle
  | Level
  | Highlight
  | Addition
  | Removal

type message = {
    style: style
  ; msg: string
}

class multi_span ?(primary_spans = new vec) ?(labels = new vec) () =
  object (self)
    val primary_spans : Span.t vec = primary_spans
    val span_labels : (Span.t * message) vec = labels
    method dummy = primary_spans#all (fun s -> s.lo = 0 && s.hi = 0)
    method has_primary_span = not self#dummy
    method primary_span = primary_spans#first

    method has_span_labels =
      span_labels#any (fun (s, _) -> s.lo = 0 && s.hi = 0)
  end

let multi_span span =
  let primary_spans = new vec in
  primary_spans#push span;
  let ms = new multi_span ~primary_spans () in
  ms
;;

class diagnostic level message multi_span =
  object
    val level : level = level
    val message : message vec = message
    val span : multi_span = multi_span

    (* getters *)
    method level = level
    method message = message
    method span = span
  end

let mk_err msg span =
  let messages = new vec in
  messages#push { msg; style = NoStyle };
  new diagnostic Err messages (multi_span span)
;;
