open Ast
open Ty
open Token
open Tokenizer
open Errors
open Diagnostic
open Session

type parse_ctx = {
  ctx : Context.t;
  tokenizer : tokenizer;
  src : string;
  mutable curr_tok : token;
  mutable prev_tok : token option;
  mutable stop : bool;
  mutable extern_block : bool;
  mutable node_id : node_id;
  emitter : Emitter.t;
}

let span (start : pos) (pctx : parse_ctx) =
  match pctx.prev_tok with
  | Some t -> { start; ending = t.span.ending }
  | None -> assert false

let advance pctx =
  match next pctx.tokenizer with
  | Some { kind = Eof; _ } -> pctx.stop <- true
  | Some t ->
      pctx.prev_tok <- Some pctx.curr_tok;
      pctx.curr_tok <- t
  | None -> assert false

let unexpected_token pctx expected t =
  let msg =
    Printf.sprintf "expected `%s`, found `%s`"
      (display_token_kind expected)
      (display_token_kind t.kind)
  in
  let span = pctx.curr_tok.span in
  {
    level = Err;
    message = "unexpected token";
    span = { primary_spans = [span]; labels = [(span, msg, true)] };
    children = [];
    sugg = [];
    loc = Diagnostic.dg_loc_from_span span;
  }

let unexpected_type pctx span =
  Emitter.emit pctx.emitter
    {
      level = Err;
      message = "unexpected type";
      span = { primary_spans = [span]; labels = [] };
      children = [];
      sugg = [];
      loc = Diagnostic.dg_loc_from_span span;
    };
  exit 1

let eat pctx kind =
  if pctx.curr_tok.kind == kind then (
    let t = pctx.curr_tok in
    advance pctx; t)
  else (
    Emitter.emit pctx.emitter (unexpected_token pctx kind pctx.curr_tok);
    exit 1)

let gen_id pctx : node_id =
  pctx.node_id <- pctx.node_id + 1;
  pctx.node_id

let parse_ctx_create ctx tokenizer s =
  match next tokenizer with
  | Some t ->
      {
        ctx;
        tokenizer;
        src = s;
        curr_tok = t;
        prev_tok = None;
        stop = false;
        extern_block = false;
        node_id = 0;
        emitter =
          { ctx; source = Array.of_list (String.split_on_char '\n' s) };
      }
  | None -> exit 0

let strip_comments pctx =
  let rec impl = function
    | Comment None -> advance pctx; impl pctx.curr_tok.kind
    | _ -> ()
  in
  impl pctx.curr_tok.kind

let parse_ident pctx = get_token_str (eat pctx Ident) pctx.src

let parse_attr pctx : normal_attr =
  ignore (eat pctx LBracket);
  let ident = parse_ident pctx in
  ignore (eat pctx RBracket);
  { name = ident }

let parse_outer_attrs pctx : attr list =
  let unexpected_inner_attr () =
    Emitter.emit pctx.emitter
      {
        level = Err;
        message = "unexpected inner attribute after outer attribute";
        span = { primary_spans = [pctx.curr_tok.span]; labels = [] };
        children = [];
        sugg = [];
        loc = Diagnostic.dg_loc_from_span pctx.curr_tok.span;
      };
    exit 1
  in
  let rec parse_outer_attrs_impl () =
    let attr_kind =
      match pctx.curr_tok.kind with
      | Bang -> unexpected_inner_attr ()
      | LBracket -> Some (NormalAttr (parse_attr pctx))
      | Comment style -> (
        match style with
        | Some Outer ->
            let outer = get_token_str pctx.curr_tok pctx.src in
            advance pctx; Some (Doc outer)
        | Some Inner -> unexpected_inner_attr ()
        | None -> advance pctx; None)
      | _ -> None
    in
    match attr_kind with
    | Some kind -> [{ kind; style = Outer }] @ parse_outer_attrs_impl ()
    | None -> []
  in
  parse_outer_attrs_impl ()

let parse_inner_attrs pctx : attr list =
  let rec parse_inner_attrs_impl () =
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
    | Some kind -> [{ kind; style = Inner }] @ parse_inner_attrs_impl ()
    | None -> []
  in
  parse_inner_attrs_impl ()

let rec parse_ty pctx : ty =
  let t = pctx.curr_tok in
  match t.kind with
  | Star ->
      advance pctx;
      Ptr (parse_ty pctx)
  | Ampersand ->
      advance pctx;
      RefTy (parse_ty pctx)
  | Ident -> (
    match get_token_str (eat pctx Ident) pctx.src with
    | "i8" -> Int I8
    | "i16" -> Int I16
    | "i32" -> Int I32
    | "i64" -> Int I64
    | "isize" -> Int Isize
    | "u8" -> Int U8
    | "u16" -> Int U16
    | "u32" -> Int U32
    | "u64" -> Int U64
    | "usize" -> Int Usize
    | "f32" -> Float F32
    | "f64" -> Float F64
    | "bool" -> Bool
    | "str" -> Str
    | _ -> unexpected_type pctx t.span)
  | _ -> unexpected_type pctx t.span

let parse_fn_args pctx : (ty * ident) list * bool =
  assert (pctx.curr_tok.kind == LParen);
  ignore (eat pctx LParen);
  let arg_list = ref [] in
  let is_variadic = ref false in
  while (not pctx.stop) && pctx.curr_tok.kind <> RParen do
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
        | RParen -> ()
        | _ -> assert false)
    | Dot3 ->
        advance pctx;
        is_variadic := true
    | _ -> assert false
  done;
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
  let rec parse_path_impl () =
    match pctx.curr_tok.kind with
    | Ident -> (
        let segment = [parse_ident pctx] in
        match pctx.curr_tok.kind with
        | Colon2 ->
            advance pctx;
            segment @ parse_path_impl ()
        | _ -> segment)
    | _ ->
        Emitter.emit pctx.emitter (unexpected_token pctx Ident pctx.curr_tok);
        exit 1
  in
  { segments = parse_path_impl () }

let parse_pat pctx : pat =
  let kind = pctx.curr_tok.kind in
  match kind with
  | Ident -> PatIdent (get_token_str (eat pctx kind) pctx.src)
  | _ -> assert false

let rec parse_expr pctx : expr = strip_comments pctx; parse_precedence pctx 0

and parse_call_args pctx : expr list =
  let args = ref [] in
  ignore (eat pctx LParen);
  while (not pctx.stop) && pctx.curr_tok.kind <> RParen do
    args := !args @ [parse_expr pctx];
    match pctx.curr_tok.kind with
    | Comma -> advance pctx
    | RParen -> ()
    | _ ->
        Emitter.emit pctx.emitter
          (unexpected_token pctx RParen pctx.curr_tok)
  done;
  ignore (eat pctx RParen);
  !args

and should_continue_as_binary_expr pctx expr =
  let is_block =
    match expr.expr_kind with If _ | Block _ -> true | _ -> false
  in
  match (is_block, pctx.curr_tok.kind) with
  | true, Star ->
      Printf.printf "\x1b[31;1m%s\x1b[0m: expression is ambiguous\n"
        (display_span expr.expr_span);
      false
  | false, _ -> true
  | _, _ -> true

and prec = function
  | Star | Slash -> 70
  | Plus | Minus -> 60
  | EqEq | BangEq -> 30
  | _ -> -1

and parse_precedence pctx min_prec : expr =
  let s = pctx.curr_tok.span.start in
  let left = ref (parse_primary pctx) in
  let p = ref (prec pctx.curr_tok.kind) in
  if should_continue_as_binary_expr pctx !left then (
    while
      (p := prec pctx.curr_tok.kind;
       !p)
      > min_prec
    do
      let kind = binary_kind_from_token pctx.curr_tok.kind in
      advance pctx;
      let right = parse_precedence pctx (!p + 1) in
      left :=
        {
          expr_kind = Binary (kind, !left, right);
          expr_ty = None;
          expr_id = gen_id pctx;
          expr_span = span s pctx;
        }
    done;
    !left)
  else !left

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
    | If -> If (parse_if pctx)
    | LBrace -> Block (parse_block pctx)
    | _ -> parse_path_or_call pctx
  in
  {
    expr_kind;
    expr_ty = None;
    expr_id = gen_id pctx;
    expr_span = span s pctx;
  }

(* parses else expr when `else` token is already eaten *)
and parse_else pctx : expr option =
  let s = pctx.curr_tok.span.start in
  let kind =
    match pctx.curr_tok.kind with
    | If -> Some (Ast.If (parse_if pctx))
    | LBrace -> Some (Block (parse_block pctx))
    | _ -> None
  in
  if Option.is_none kind then None
  else
    Some
      {
        expr_kind = Option.get kind;
        expr_ty = None;
        expr_id = gen_id pctx;
        expr_span = span s pctx;
      }

and parse_if pctx =
  advance pctx;
  let cond = parse_expr pctx in
  let then_block = parse_block pctx in
  let else_block =
    if pctx.curr_tok.kind = Else then (advance pctx; parse_else pctx)
    else None
  in
  { cond; then_block; else_block }

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

and parse_let pctx : binding =
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

and parse_stmt pctx : stmt =
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

and parse_block pctx : block =
  let stmt_list = ref [] in
  let last_expr = ref None in
  ignore (eat pctx LBrace);
  while (not pctx.stop) && pctx.curr_tok.kind <> RBrace do
    match pctx.curr_tok.kind with
    | RBrace -> ()
    | _ -> (
        let stmt = parse_stmt pctx in
        match stmt with
        | Expr expr ->
            (match !last_expr with
            | Some expr -> stmt_list := !stmt_list @ [Stmt expr]
            | None -> ());
            last_expr := Some expr
        | stmt -> stmt_list := !stmt_list @ [stmt])
  done;
  ignore (eat pctx RBrace);
  {
    block_stmts = !stmt_list;
    last_expr = !last_expr;
    block_id = gen_id pctx;
  }

let parse_fn pctx abi is_extern : func =
  advance pctx;
  let sign = parse_fn_sig pctx in
  if pctx.curr_tok.kind == Semi then (
    advance pctx;
    {
      is_extern;
      abi;
      fn_sig = sign;
      body = None;
      func_id = gen_id pctx;
      func_path = None;
    })
  else (
    let body = parse_block pctx in
    {
      is_extern;
      abi;
      fn_sig = sign;
      body = Some body;
      func_id = gen_id pctx;
      func_path = None;
    })

let parse_extern pctx attrs : item =
  advance pctx;
  let abi =
    match pctx.curr_tok.kind with
    | Lit String ->
        let buf = get_token_str (eat pctx pctx.curr_tok.kind) pctx.src in
        let s = String.sub buf 1 (String.length buf - 2) in
        let s = Scanf.unescaped s in
        s
    | _ -> "C"
  in
  match pctx.curr_tok.kind with
  | LBrace ->
      let f () =
        advance pctx;
        let items = ref [] in
        while pctx.curr_tok.kind <> RBrace do
          items := !items @ [parse_fn pctx abi true]
        done;
        ignore (eat pctx RBrace);
        !items
      in
      Foreign (f ())
  | Fn -> Fn (parse_fn pctx abi true, attrs)
  | _ -> assert false

let parse_item pctx : item =
  let attrs = parse_outer_attrs pctx in
  match pctx.curr_tok.kind with
  | Fn -> Fn (parse_fn pctx "C" false, attrs)
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
    match Filename.remove_extension (Filename.basename mod_path) with
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
    modd.items <- modd.items @ [parse_item pctx]
  done;
  modd
