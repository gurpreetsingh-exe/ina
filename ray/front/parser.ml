open Ast
open Token
open Tokenizer

type parse_ctx = {
  tokenizer : tokenizer;
  src : string;
  mutable curr_tok : token;
  mutable prev_tok : token option;
  mutable stop : bool;
  mutable extern_block : bool;
  mutable node_id : node_id;
}

let span (start : pos) (pctx : parse_ctx) =
  match pctx.prev_tok with
  | Some t -> { start; ending = t.span.ending }
  | None -> assert false

type perr = UnexpectedToken of (string * span)

exception ParseError of perr

let advance pctx =
  match next pctx.tokenizer with
  | Some { kind = Eof; _ } -> pctx.stop <- true
  | Some t ->
      pctx.prev_tok <- Some pctx.curr_tok;
      pctx.curr_tok <- t
  | None -> assert false

let unexpected_token _pctx expected t =
  ParseError
    (UnexpectedToken
       ( Printf.sprintf " expected `%s` found `%s`"
           (display_token_kind expected)
           (display_token_kind t.kind),
         t.span ))

let eat pctx kind =
  if pctx.curr_tok.kind == kind then (
    let t = pctx.curr_tok in
    advance pctx; t)
  else raise (unexpected_token pctx kind pctx.curr_tok)

let gen_id pctx : node_id =
  pctx.node_id <- pctx.node_id + 1;
  pctx.node_id

let parse_ctx_create tokenizer s =
  match next tokenizer with
  | Some t ->
      {
        tokenizer;
        src = s;
        curr_tok = t;
        prev_tok = None;
        stop = false;
        extern_block = false;
        node_id = 0;
      }
  | None -> exit 0

let emit_err e =
  match e with
  | UnexpectedToken (msg, span) ->
      let filename, _, line, col = span.start in
      Printf.fprintf stderr "%s:%d:%d %s\n" filename line col msg;
      exit 1

let strip_comments pctx =
  try
    while true do
      match pctx.curr_tok.kind with
      | Comment None -> advance pctx
      | _ -> raise Exit
    done
  with Exit -> ()

let parse_ident pctx = get_token_str (eat pctx Ident) pctx.src

let parse_attr pctx : normal_attr =
  ignore (eat pctx LBracket);
  let ident = parse_ident pctx in
  ignore (eat pctx RBracket);
  { name = ident }

let parse_outer_attrs pctx : attr list =
  let attrs = ref [] in
  try
    while not pctx.stop do
      let attr_kind =
        match pctx.curr_tok.kind with
        | Bang ->
            raise
              (ParseError
                 (UnexpectedToken
                    ( "unexpected inner attribute after outer attribute",
                      pctx.curr_tok.span )))
        | LBracket -> Some (NormalAttr (parse_attr pctx))
        | Comment style -> (
          match style with
          | Some Outer ->
              let outer = get_token_str pctx.curr_tok pctx.src in
              advance pctx; Some (Doc outer)
          | Some Inner ->
              raise
                (ParseError
                   (UnexpectedToken
                      ( "unexpected inner attribute after outer attribute",
                        pctx.curr_tok.span )))
          | None -> advance pctx; None)
        | _ -> None
      in
      match attr_kind with
      | Some kind -> attrs := !attrs @ [{ kind; style = Outer }]
      | None -> raise Exit
    done;
    !attrs
  with Exit -> !attrs

let parse_inner_attrs pctx : attr list =
  let attrs = ref [] in
  try
    while not pctx.stop do
      let attr_kind =
        match pctx.curr_tok.kind with
        | Bang ->
            advance pctx;
            Some (NormalAttr (parse_attr pctx))
        | Comment style -> (
          match style with
          | Some Inner ->
              let inner = get_token_str pctx.curr_tok pctx.src in
              advance pctx; Some (Doc inner)
          | Some Outer -> None
          | None -> advance pctx; None)
        | _ -> None
      in
      match attr_kind with
      | Some kind -> attrs := !attrs @ [{ kind; style = Inner }]
      | None -> raise Exit
    done;
    !attrs
  with Exit -> !attrs

let rec parse_ty pctx : ty =
  let t = pctx.curr_tok in
  match t.kind with
  | Star ->
      advance pctx;
      Ptr (parse_ty pctx)
  | Ampersand ->
      advance pctx;
      RefTy (parse_ty pctx)
  | Ident ->
      Prim
        (match get_token_str (eat pctx Ident) pctx.src with
        | "i8" -> I8
        | "i16" -> I16
        | "i32" -> I32
        | "i64" -> I64
        | "isize" -> Isize
        | "u8" -> U8
        | "u16" -> U16
        | "u32" -> U32
        | "u64" -> U64
        | "usize" -> Usize
        | "f32" -> F32
        | "f64" -> F64
        | "bool" -> Bool
        | "str" -> Str
        | _ -> raise (unexpected_token pctx Ident t))
  | _ -> raise (unexpected_token pctx Ident t)

let parse_fn_args pctx : (ty * ident) list * bool =
  assert (pctx.curr_tok.kind == LParen);
  ignore (eat pctx LParen);
  let arg_list = ref [] in
  let is_variadic = ref false in
  (try
     while not pctx.stop do
       if pctx.curr_tok.kind = RParen then raise Exit;
       match pctx.curr_tok.kind with
       | Ident -> (
           let arg =
             let ident = parse_ident pctx in
             ignore (eat pctx Colon);
             let ty = parse_ty pctx in
             (ty, ident)
           in
           arg_list := !arg_list @ [arg];
           match pctx.curr_tok.kind with
           | Comma -> advance pctx
           | RParen -> raise Exit
           | _ -> assert false)
       | Dot3 ->
           advance pctx;
           is_variadic := true;
           raise Exit
       | _ -> assert false
     done
   with Exit -> ());
  ignore (eat pctx RParen);
  (!arg_list, !is_variadic)

let parse_ret_ty pctx : ty option =
  match pctx.curr_tok.kind with
  | Arrow ->
      advance pctx;
      Some (parse_ty pctx)
  | _ -> None

let parse_fn_sig pctx : fn_sig =
  let s = pctx.curr_tok.span.start in
  let ident = parse_ident pctx in
  let args, is_variadic = parse_fn_args pctx in
  let ret_ty = parse_ret_ty pctx in
  { name = ident; args; ret_ty; fn_span = span s pctx; is_variadic }

let parse_path pctx : path =
  let segments =
    let s = ref [] in
    try
      while pctx.curr_tok.kind = Ident do
        s := !s @ [parse_ident pctx];
        match pctx.curr_tok.kind with
        | Colon2 -> advance pctx
        | _ -> raise Exit
      done;
      !s
    with Exit -> !s
  in
  { segments }

let rec parse_expr pctx : expr = strip_comments pctx; parse_addition pctx

and parse_call_args pctx : expr list =
  let args = ref [] in
  ignore (eat pctx LParen);
  (try
     while pctx.curr_tok.kind <> RParen do
       args := !args @ [parse_expr pctx];
       match pctx.curr_tok.kind with
       | RParen -> raise Exit
       | Comma -> advance pctx
       | kind -> ignore (eat pctx kind)
     done
   with Exit -> ());
  ignore (eat pctx RParen);
  !args

and parse_binary pctx (rule : parse_ctx -> expr)
    (binary_op : parse_ctx -> bool) : expr =
  let s = pctx.curr_tok.span.start in
  let left = ref (rule pctx) in
  (try
     while true do
       if binary_op pctx then (
         let kind = binary_kind_from_token pctx.curr_tok.kind in
         advance pctx;
         let right = rule pctx in
         left :=
           {
             expr_kind = Binary (kind, !left, right);
             expr_ty = None;
             expr_id = gen_id pctx;
             expr_span = span s pctx;
           })
       else raise Exit
     done
   with Exit -> ());
  {
    expr_kind = !left.expr_kind;
    expr_ty = None;
    expr_id = gen_id pctx;
    expr_span = span s pctx;
  }

and parse_multiply pctx : expr =
  parse_binary pctx parse_primary (fun pctx ->
      match pctx.curr_tok.kind with Star | Slash -> true | _ -> false)

and parse_addition pctx : expr =
  parse_binary pctx parse_multiply (fun pctx ->
      match pctx.curr_tok.kind with Plus | Minus -> true | _ -> false)

and parse_primary pctx : expr =
  let s = pctx.curr_tok.span.start in
  let expr_kind =
    match pctx.curr_tok.kind with
    | Star ->
        advance pctx;
        Deref (parse_expr pctx)
    | Ampersand ->
        advance pctx;
        Ref (parse_expr pctx)
    | Lit lit as kind ->
        let buf = get_token_str (eat pctx kind) pctx.src in
        Ast.Lit
          (match lit with
          | Int -> LitInt (int_of_string buf)
          | Float -> LitFloat (float_of_string buf)
          | Bool -> LitBool (bool_of_string buf)
          | String ->
              let s = String.sub buf 1 (String.length buf - 2) in
              let s = Scanf.unescaped s in
              LitStr s
          | lit_kind ->
              ignore (Printf.printf "%s\n" (display_literal lit_kind));
              assert false)
    | _ -> parse_path_or_call pctx
  in
  {
    expr_kind;
    expr_ty = None;
    expr_id = gen_id pctx;
    expr_span = span s pctx;
  }

and parse_path_or_call pctx =
  match pctx.curr_tok.kind with
  | Ident -> (
      let path = parse_path pctx in
      match pctx.curr_tok.kind with
      | LParen -> Call (path, parse_call_args pctx)
      | _ -> Path path)
  | kind ->
      ignore (Printf.printf "%s\n" (display_token_kind kind));
      assert false

let parse_pat pctx : pat =
  let kind = pctx.curr_tok.kind in
  match kind with
  | Ident -> PatIdent (get_token_str (eat pctx kind) pctx.src)
  | _ -> assert false

let parse_let pctx : binding =
  ignore (eat pctx Let);
  let binding_create pat ty =
    ignore (eat pctx Eq);
    let binding_expr = parse_expr pctx in
    ignore (eat pctx Semi);
    {
      binding_pat = pat;
      binding_ty = ty;
      binding_expr;
      binding_id = gen_id pctx;
    }
  in
  let pat = parse_pat pctx in
  match pctx.curr_tok.kind with
  | Eq -> binding_create pat None
  | Colon ->
      advance pctx;
      binding_create pat (Some (parse_ty pctx))
  | _ -> assert false

let parse_stmt pctx : stmt =
  if pctx.curr_tok.kind = Let then Binding (parse_let pctx)
  else (
    let expr = parse_expr pctx in
    match pctx.curr_tok.kind with
    | Semi -> advance pctx; Stmt expr
    | Eq ->
        advance pctx;
        let init = parse_expr pctx in
        ignore (eat pctx Semi);
        Assign (expr, init)
    | _ -> Expr expr)

let parse_block pctx : block =
  ignore (eat pctx LBrace);
  let stmt_list = ref [] in
  let last_expr = ref None in
  (try
     while not pctx.stop do
       let stmt =
         match pctx.curr_tok.kind with
         | RBrace -> raise Exit
         | _ -> parse_stmt pctx
       in
       match stmt with
       | Expr expr -> last_expr := Some expr
       | stmt -> stmt_list := !stmt_list @ [stmt]
     done
   with Exit -> ());
  ignore (eat pctx RBrace);
  {
    block_stmts = !stmt_list;
    last_expr = !last_expr;
    block_id = gen_id pctx;
  }

let parse_fn pctx : func =
  let is_extern =
    match pctx.prev_tok with Some { kind = Extern; _ } -> true | _ -> false
  in
  advance pctx;
  let sign = parse_fn_sig pctx in
  if pctx.curr_tok.kind == Semi then (
    advance pctx;
    {
      is_extern;
      fn_sig = sign;
      body = None;
      func_id = gen_id pctx;
      func_path = None;
    })
  else (
    let body = parse_block pctx in
    {
      is_extern;
      fn_sig = sign;
      body = Some body;
      func_id = gen_id pctx;
      func_path = None;
    })

let parse_extern pctx attrs : item =
  advance pctx;
  Fn (parse_fn pctx, attrs)

let parse_item pctx : item =
  let attrs = parse_outer_attrs pctx in
  match pctx.curr_tok.kind with
  | Fn -> Fn (parse_fn pctx, attrs)
  | Extern -> parse_extern pctx attrs
  | Import ->
      advance pctx;
      let import = Ast.Import (parse_path pctx) in
      ignore (eat pctx Semi);
      import
  | kind ->
      Printf.printf "%s\n" (display_token_kind kind);
      assert false

let parse_mod pctx : modd =
  let mod_attrs = parse_inner_attrs pctx in
  let mod_path = pctx.tokenizer.filename in
  let mod_name =
    match Filename.chop_extension (Filename.basename mod_path) with
    | "lib" -> Filename.basename (Filename.dirname mod_path)
    | other -> other
  in
  let modd =
    {
      items = [];
      attrs = mod_attrs;
      mod_name;
      mod_path;
      mod_id = gen_id pctx;
      imported_mods = Hashtbl.create 0;
    }
  in
  while not pctx.stop do
    strip_comments pctx;
    try modd.items <- modd.items @ [parse_item pctx]
    with ParseError err -> emit_err err
  done;
  modd
