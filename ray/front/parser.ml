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
}

type perr = UnexpectedToken of (string * span)

exception ParseError of perr

let advance pctx =
  match next pctx.tokenizer with
  | Some { kind = Eof; _ } -> pctx.stop <- true
  | Some t ->
      pctx.prev_tok <- Some pctx.curr_tok;
      pctx.curr_tok <- t
  | None -> assert false

let eat pctx kind =
  if pctx.curr_tok.kind == kind then (
    let t = pctx.curr_tok in
    advance pctx; t)
  else
    raise
      (ParseError
         (UnexpectedToken
            ( "expected " ^ display_token_kind kind ^ "found "
              ^ display_token_kind pctx.curr_tok.kind,
              pctx.curr_tok.span )))

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
      }
  | None -> exit 0

let emit_err e =
  match e with
  | UnexpectedToken (msg, span) ->
      let filename, pos = span.start in
      Printf.fprintf stderr "%s:%d:%s\n" filename pos msg;
      exit 1

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
        | LBracket -> Some (NormalAttr (parse_attr pctx))
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

let parse_fn_args pctx : (ty * ident option) list =
  assert (pctx.curr_tok.kind == LParen);
  ignore (eat pctx LParen);
  ignore (eat pctx RParen);
  []

let parse_ret_ty _ : ty option = None

let parse_fn_sig pctx : fn_sig =
  let ident = parse_ident pctx in
  let args = parse_fn_args pctx in
  let ret_ty = parse_ret_ty pctx in
  { name = ident; args; ret_ty }

let parse_block pctx : block =
  assert (pctx.curr_tok.kind == LBrace);
  ignore (eat pctx LBrace);
  ignore (eat pctx RBrace);
  { block_stmts = []; last_expr = None }

let parse_fn pctx : func =
  let is_extern =
    match pctx.prev_tok with Some { kind = Extern; _ } -> true | _ -> false
  in
  advance pctx;
  let sign = parse_fn_sig pctx in
  if pctx.curr_tok.kind == Semi then (
    advance pctx;
    { is_extern; fn_sig = sign; body = None })
  else (
    let body = parse_block pctx in
    { is_extern; fn_sig = sign; body = Some body })

let parse_extern pctx attrs : item =
  advance pctx;
  Fn (parse_fn pctx, attrs)

let parse_item pctx : item =
  let attrs = parse_outer_attrs pctx in
  match pctx.curr_tok.kind with
  | Fn -> Fn (parse_fn pctx, attrs)
  | Extern -> parse_extern pctx attrs
  | kind ->
      Printf.printf "%s\n" (display_token_kind kind);
      assert false

let parse_mod pctx : modd =
  let mod_attrs = parse_inner_attrs pctx in
  let modd = { items = []; attrs = mod_attrs } in
  while not pctx.stop do
    try modd.items <- modd.items @ [parse_item pctx]
    with ParseError err -> emit_err err
  done;
  modd
