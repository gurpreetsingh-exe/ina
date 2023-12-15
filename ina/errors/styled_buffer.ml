open Structures.Vec

type style =
  | LineCol
  | LineNum
  | NoStyle
  | Header
  | Level of Diagnostic.level

type styled_char = {
    mutable chr: char
  ; mutable style: style
}

type styled_string = {
    mutable text: string
  ; mutable style: style
}

class styled_buffer =
  object (self)
    val lines : styled_char vec vec = new vec

    method ensure_lines line =
      if line >= lines#len then lines#resize (line + 1) (new vec)

    method putc line col char style =
      self#ensure_lines line;
      if col >= (lines#get line)#len
      then (lines#get line)#resize (col + 1) { chr = ' '; style = NoStyle };
      (lines#get line)#set col { chr = char; style }

    method puts line col str style =
      let n = ref col in
      String.iter
        (fun c ->
          self#putc line !n c style;
          incr n)
        str

    method append line str style =
      if line >= lines#len
      then self#puts line 0 str style
      else
        let col = (lines#get line)#len in
        self#puts line col str style

    method prepend line str style =
      self#ensure_lines line;
      let l = String.length str in
      if not (lines#get line)#empty
      then
        for _ = 0 to l do
          (lines#get line)#insert 0 { chr = ' '; style = NoStyle }
        done;
      self#puts line 0 str style

    method render =
      let fold_line acc { chr; style } =
        let text = String.make 1 chr in
        (match acc#last with
         | Some str when str.style = style ->
             str.text <- String.concat "" [str.text; text]
         | _ -> acc#push { text; style });
        acc
      in
      fold_left
        (fun acc str ->
          acc#push (fold_left fold_line (new vec) str);
          acc)
        (new vec)
        lines
  end
