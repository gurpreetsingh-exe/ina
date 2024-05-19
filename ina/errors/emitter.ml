open Printf
open Diagnostic
open Label
open Source
open Color
open Source.Source_map

type t = {
    sm: source_map option
  ; ui_testing: bool
}

type line =
  | Header of (level * string)
  | LineNumber of int
  | SourceLine of (int * Label.t list)
  | NestingBegin of (int * Label.t)
  | NestingEnd of (int * Label.t)
  | Annotation of (int * Label.t list)
  | Message of (int * bool * Label.t)
  | AnnotationAnchor of (int * Label.t list)
  | MultispanMessage of (int * Label.t list * Label.t)
  | Blank of (int * bool)
  | Footer

let repeat ?(c = " ") n =
  if n < 0 then "" else List.init n (fun _ -> c) |> String.concat ""
;;

let ustring =
  Uutf.String.fold_utf_8
    (fun acc _ -> function
      | `Uchar u -> Array.append acc [|u|]
      | _ -> assert false)
    [||]
;;

type chars = {
    hbar: string
  ; vbar: string
  ; ltop: string
  ; rtop: string
  ; lbot: string
  ; rbot: string
  ; t: string
  ; lt: string
  ; arrow: string
  ; halfslab: string
  ; dot: string
}

let chars =
  {
    hbar = "─"
  ; vbar = "│"
  ; ltop = "╭"
  ; rtop = "╮"
  ; lbot = "╰"
  ; rbot = "╯"
  ; t = "┬"
  ; lt = "├"
  ; arrow = "➤"
  ; halfslab = "╶"
  ; dot = "∙" (* ; dot = "⋅" *)
  }
;;

let max_span =
  let open Span in
  List.fold_left (fun acc span -> max acc span.hi) 0
  $ List.map (fun Label.{ span; _ } -> span)
;;

class emitter sm ui_testing =
  object (self)
    val sm : source_map option = sm
    val ui_testing : bool = ui_testing
    initializer Color.disable := ui_testing

    (* public methods *)
    method emit_diagnostic = self#emit_messages

    (* private methods *)
    method private emit_messages diagnostic =
      let max_span = max_span diagnostic.labels in
      let c = level_to_color diagnostic.level in
      match sm with
      | Some sm ->
          let open Span in
          let annotation span =
            let _, column = sm#lookup_line_pos span.lo in
            let padding = repeat column in
            let range = Span.range span in
            (if range = 2
             then [padding; chars.halfslab; chars.rtop]
             else
               let hrange = range / 2 in
               let head = repeat ~c:chars.hbar hrange in
               let tail = repeat ~c:chars.hbar (range - hrange - 1) in
               [padding; head; chars.t; tail])
            |> String.concat ""
          in
          let message span =
            let _, column = sm#lookup_line_pos span.lo in
            let range = Span.range span in
            let hrange = range / 2 in
            let head = repeat hrange in
            let padding = repeat column in
            padding ^ head ^ chars.vbar
          in
          let ustring_to_str ustr =
            let b = Buffer.create 0 in
            Array.iter
              (fun (c, u) ->
                let b' = Buffer.create 0 in
                Array.iter (fun u -> Uutf.Buffer.add_utf_8 b' u) u;
                Buffer.add_string b (format [c, Buffer.contents b']))
              ustr;
            Buffer.contents b
          in
          let fold_annotation labels f =
            List.fold_left
              (fun acc label ->
                let buf =
                  Array.fold_left
                    (fun acc (_, buf) -> Array.append acc buf)
                    [||]
                    acc
                in
                let span = label.span in
                let buf' = f span in
                let buf' = ustring buf' in
                Array.append
                  acc
                  [|
                     ( (if Label.is_primary label then c else label.color)
                     , Array.sub
                         buf'
                         (Array.length buf)
                         (Array.length buf' - Array.length buf) )
                  |])
              [||]
              labels
          in
          let line_num_len line =
            line + 1 |> string_of_int |> String.length
          in
          let line, _ = sm#lookup_line_pos max_span in
          let max_line_num_len = line_num_len line in
          let padding = String.make (1 + max_line_num_len) ' ' in
          let labels =
            List.fast_sort
              (fun l1 l2 -> l1.span.lo - l2.span.lo)
              diagnostic.labels
          in
          let nest = ref 0 in
          let nest_begin =
            List.filter
              (fun label -> not @@ sm#is_same_line label.span)
              labels
            |> List.map (fun label ->
                   let line, _ = sm#lookup_line_pos label.span.lo in
                   line)
          in
          let single_lines =
            List.filter (fun label -> sm#is_same_line label.span) labels
          in
          let multispans = Hashtbl.create 0 in
          List.iter
            (fun label ->
              let span = label.span in
              let line, _ = sm#lookup_line_pos span.lo in
              match Hashtbl.find_opt multispans line with
              | Some n -> Hashtbl.replace multispans line (n @ [label])
              | None -> Hashtbl.add multispans line [label])
            single_lines;
          let nest_end = ref [] in
          let nest_msg = Hashtbl.create 0 in
          let nesting = repeat ~c:(" " ^ chars.vbar ^ " ") in
          let nlabels = List.length labels in
          (([Header (diagnostic.level, diagnostic.message)]
            :: (Diagnostic.primary diagnostic.labels |> function
                | Some primary ->
                    [LineNumber primary.span.lo; Blank (0, true)]
                | None -> [])
            :: List.mapi
                 (fun i label ->
                   let last = i = nlabels - 1 in
                   let line, _ = sm#lookup_line_pos label.span.lo in
                   let i = !nest in
                   match sm#is_same_line label.span with
                   | true when List.mem line nest_begin ->
                       [
                         Annotation (i, [label])
                       ; Message (i, false, label)
                       ; Blank (i, false)
                       ]
                   | true ->
                       (match Hashtbl.find_opt multispans line with
                        | Some [] -> assert false
                        | Some [label] ->
                            Hashtbl.remove multispans line;
                            [
                              SourceLine (i, [label])
                            ; Annotation (i, [label])
                            ; Message (i, false, label)
                            ; Blank (i, false)
                            ]
                        | Some labels ->
                            let rec f = function
                              | [] -> []
                              | labels ->
                                  let label = List.hd labels in
                                  let labels = List.tl labels in
                                  [
                                    MultispanMessage
                                      (i, List.rev labels, label)
                                  ; AnnotationAnchor (i, List.rev labels)
                                  ]
                                  @ f labels
                            in
                            let anchors = f (labels |> List.rev) in
                            Hashtbl.remove multispans line;
                            [SourceLine (i, labels); Annotation (i, labels)]
                            @ anchors
                            @ [Blank (i, false)]
                        | None -> [])
                       @
                       (match !nest_end with
                        | x :: rest when x < label.span.hi || last ->
                            decr nest;
                            let col =
                              sm#lookup_line_src x |> String.length
                            in
                            nest_end := rest;
                            let label = Hashtbl.find nest_msg x in
                            let label' =
                              Label.{ label with span = Span.make 0 col }
                            in
                            [
                              NestingEnd
                                ( i - 1
                                , {
                                    label' with
                                    span = Span.make x (x + col)
                                  } )
                            ; Blank (i, true)
                            ; Message (i - 1, true, label')
                            ; Blank (i - 1, false)
                            ]
                        | _ -> [])
                   | false ->
                       incr nest;
                       Hashtbl.add nest_msg label.span.hi label;
                       nest_end := label.span.hi :: !nest_end;
                       let x = label.span.hi in
                       let col = sm#lookup_line_src x |> String.length in
                       [NestingBegin (i, label)]
                       @
                       if last
                       then
                         let label' =
                           Label.{ label with span = Span.make 0 col }
                         in
                         [
                           Blank (i + 1, false)
                         ; NestingEnd
                             (i, { label' with span = Span.make x (x + col) })
                         ; Blank (i + 1, true)
                         ; Message (i - 1, true, label')
                         ; Blank (i - 1, false)
                         ]
                       else [Blank (i + 1, false)])
                 labels)
           @ if List.length labels = 0 then [] else [[Footer]])
          |> List.fold_left ( @ ) []
          |> List.map (fun line ->
                 (match line with
                  | Header _ | LineNumber _ | Footer -> ""
                  | SourceLine (_, label :: _)
                  | NestingBegin (_, label)
                  | NestingEnd (_, label) ->
                      let line = label.span.lo in
                      let linestart, _ = sm#lookup_line_pos line in
                      let line_num_len = line_num_len linestart in
                      let padding =
                        String.make (max_line_num_len - line_num_len) ' '
                      in
                      sprintf
                        " %s%d %s"
                        padding
                        (linestart + 1)
                        (format [margin, chars.vbar])
                  | Blank (_, end') when not end' ->
                      sprintf " %s%s" padding (format [margin, chars.dot])
                  | _ ->
                      sprintf " %s%s" padding (format [margin, chars.vbar]))
                 ^ (line |> function
                    | Header (level, msg) ->
                        format
                          [
                            Bold, ""
                          ; c, display_level level
                          ; Normal, ": "
                          ; Bold, msg
                          ]
                    | LineNumber lo ->
                        format
                          [
                            margin, padding ^ " "
                          ; margin, chars.ltop ^ chars.hbar
                          ; margin, "[" ^ sm#span_to_string lo ^ "]"
                          ]
                    | Annotation (nest, [label]) ->
                        let span = label.span in
                        let c =
                          if Label.is_primary label then c else label.color
                        in
                        let ann = format [Bold, ""; c, annotation span] in
                        let nest = nesting nest in
                        sprintf "%s %s" nest ann
                    | Annotation (nest, labels) ->
                        let buf = fold_annotation labels annotation in
                        let nest = nesting nest in
                        sprintf
                          "%s %s"
                          nest
                          (format [Bold, ustring_to_str buf])
                    | MultispanMessage (nest, labels, label) ->
                        let acc = fold_annotation labels message in
                        let _, col = sm#lookup_line_pos label.span.lo in
                        let spansize = Span.range label.span in
                        let halfspan = spansize / 2 in
                        let head = repeat ~c:chars.hbar halfspan in
                        let ann = [chars.lbot; head] |> String.concat "" in
                        let pad = repeat (col + halfspan) in
                        let buf =
                          Array.fold_left
                            (fun acc (_, buf) -> Array.append acc buf)
                            [||]
                            acc
                        in
                        let buf' = pad ^ ann in
                        let buf' = ustring buf' in
                        let buf' =
                          Array.sub
                            buf'
                            (Array.length buf)
                            (Array.length buf' - Array.length buf)
                        in
                        let c =
                          if Label.is_primary label then c else label.color
                        in
                        let buf = Array.append acc [|c, buf'|] in
                        let nest = nesting nest in
                        let m =
                          format
                            [
                              Bold, ""
                            ; c, repeat ~c:chars.hbar 2
                            ; Normal, " "
                            ; Bold, label.message
                            ]
                        in
                        sprintf
                          "%s %s%s"
                          nest
                          (format [Bold, ustring_to_str buf])
                          m
                    | AnnotationAnchor (nest, labels) ->
                        let buf = fold_annotation labels message in
                        let nest = nesting nest in
                        sprintf
                          "%s %s"
                          nest
                          (format [Bold, ustring_to_str buf])
                    | Blank (nest, _end') ->
                        let nest = nesting nest in
                        sprintf "%s" nest
                    | Message (nest, end', label) ->
                        let nest = nesting nest in
                        let { span; message = msg; color; _ } = label in
                        let c =
                          if Label.is_primary label then c else color
                        in
                        if not end'
                        then
                          let _, col = sm#lookup_line_pos span.lo in
                          let spansize = Span.range span in
                          let halfspan = (spansize / 2) + 1 in
                          let head = repeat ~c:chars.hbar halfspan in
                          let pad = repeat (col + halfspan) in
                          let m = String.concat "" [pad; chars.lbot; head] in
                          let m =
                            format [Bold, ""; c, m; Normal, " "; Bold, msg]
                          in
                          sprintf "%s%s" nest m
                        else
                          sprintf
                            " %s%s%s %s"
                            nest
                            chars.lbot
                            (repeat ~c:chars.hbar (span.hi + 4))
                            (format [Bold, msg])
                    | SourceLine (nest, [label]) ->
                        let line = label.span.lo in
                        let _, start = sm#lookup_line_pos line in
                        let _, end' = sm#lookup_line_pos label.span.hi in
                        let nest = nesting nest in
                        let s = sm#lookup_line_src line in
                        let c =
                          if Label.is_primary label then c else label.color
                        in
                        let s =
                          format
                            [
                              Normal, String.sub s 0 start
                            ; Bold, ""
                            ; c, String.sub s start (end' - start)
                            ; ( Normal
                              , String.sub s end' (String.length s - end') )
                            ]
                        in
                        sprintf "%s %s" nest s
                    | SourceLine (nest, labels) ->
                        let nest = nesting nest in
                        let line = (List.hd labels).span.lo in
                        let s = sm#lookup_line_src line in
                        let s, _ =
                          List.fold_left
                            (fun (acc, i) label ->
                              let line = label.span.lo in
                              let _, start = sm#lookup_line_pos line in
                              let _, end' =
                                sm#lookup_line_pos label.span.hi
                              in
                              let c =
                                if Label.is_primary label
                                then c
                                else label.color
                              in
                              let _, s = List.rev acc |> List.hd in
                              let tl = List.rev acc |> List.tl |> List.rev in
                              let start = start - i in
                              let end' = end' - i in
                              let s =
                                [
                                  Normal, String.sub s 0 start
                                ; Bold, ""
                                ; c, String.sub s start (end' - start)
                                ; ( Normal
                                  , String.sub s end' (String.length s - end')
                                  )
                                ]
                              in
                              tl @ s, end')
                            ([Normal, s], 0)
                            labels
                        in
                        sprintf "%s %s" nest (format s)
                    | NestingBegin (nest, label) ->
                        let line = label.span.lo in
                        let nest = nesting nest in
                        let s = sm#lookup_line_src line in
                        sprintf
                          " %s%s%s%s %s"
                          nest
                          chars.ltop
                          chars.hbar
                          chars.arrow
                          s
                    | NestingEnd (nest, label) ->
                        let line = label.span.lo in
                        let nest = nesting nest in
                        let s = sm#lookup_line_src line in
                        sprintf
                          " %s%s%s%s %s"
                          nest
                          chars.lt
                          chars.hbar
                          chars.arrow
                          s
                    | Footer ->
                        let l =
                          repeat ~c:chars.hbar @@ (String.length padding + 1)
                        in
                        format [margin, l; margin, chars.rbot]))
          |> String.concat "\n"
          |> eprintf "%s\n%!"
      | None -> ()
  end
