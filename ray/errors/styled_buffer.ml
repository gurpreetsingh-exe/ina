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
      let output = new vec in
      let styled_vec = ref (new vec) in
      let process_line line =
        let curr_style = ref NoStyle in
        let curr_text = ref String.empty in
        let process_char (chr : styled_char) =
          if chr.style <> !curr_style
          then (
            if String.length !curr_text <> 0
            then !styled_vec#push { text = !curr_text; style = !curr_style };
            curr_style := chr.style;
            curr_text := String.empty);
          curr_text := String.concat "" [!curr_text; String.make 1 chr.chr]
        in
        line#iter process_char;
        if String.length !curr_text <> 0
        then !styled_vec#push { text = !curr_text; style = !curr_style };
        output#push !styled_vec;
        styled_vec := new vec
      in
      lines#iter process_line;
      output
  end
