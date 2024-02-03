open Source
open Structures.Vec

let ( $ ) f g h = f (g h)
let e = "\x1b[0m"

type level =
  | Note
  | Warn
  | Err
  | Bug
  | Help

let level_to_color = function
  | Note -> Color.Fg Cyan
  | Warn -> Color.Fg Yellow
  | Err | Bug -> Color.Fg Red
  | Help -> Color.Fg White
;;

let display_level = function
  | Note -> "note"
  | Warn -> "warning"
  | Err -> "error"
  | Bug -> "internal compiler error"
  | Help -> "help"
;;

module Priority = struct
  type t =
    | Primary
    | Secondary

  let is_primary = function Primary -> true | Secondary -> false
  let is_secondary = not $ is_primary
end

module Label = struct
  type t = {
      message: string
    ; priority: Priority.t
    ; color: Color.t
    ; span: Span.t
  }

  let primary ?(color = Color.next ()) message span =
    { priority = Primary; message; color; span }
  ;;

  let secondary ?(color = Color.Fg Cyan) message span =
    { priority = Secondary; message; color; span }
  ;;

  let is_primary label = Priority.is_primary label.priority
  let is_secondary label = Priority.is_secondary label.priority
end

type t = {
    level: level
  ; message: string
  ; labels: Label.t list
}

let create ?(level = Err) ?(labels = []) message = { level; message; labels }
let label label dg = { dg with labels = label :: dg.labels }
let primary labels = List.nth_opt (List.filter Label.is_primary labels) 0
let mk_err msg span = create ~labels:[Label.primary "" span] msg
