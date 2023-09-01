open Ast
open Ty
open Token
open Tokenizer
open Errors
open Diagnostic

let builtin_types =
  let tbl = Hashtbl.create 0 in
  Hashtbl.add tbl "i8" Ty.(Int I8);
  Hashtbl.add tbl "i16" (Int I16);
  Hashtbl.add tbl "i32" (Int I32);
  Hashtbl.add tbl "i64" (Int I64);
  Hashtbl.add tbl "isize" (Int Isize);
  Hashtbl.add tbl "u8" (Int U8);
  Hashtbl.add tbl "u16" (Int U16);
  Hashtbl.add tbl "u32" (Int U32);
  Hashtbl.add tbl "u64" (Int U64);
  Hashtbl.add tbl "usize" (Int Usize);
  Hashtbl.add tbl "f32" (Float F32);
  Hashtbl.add tbl "f64" (Float F64);
  Hashtbl.add tbl "bool" Bool;
  Hashtbl.add tbl "str" Str;
  tbl

type parse_ctx = {
  tcx : tcx;
  tokenizer : tokenizer;
  src : string;
  mutable curr_tok : token;
  mutable prev_tok : token option;
  mutable stop : bool;
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

let peek pctx =
  let loc, c = (pctx.tokenizer.pos, pctx.tokenizer.c) in
  let t =
    match next pctx.tokenizer with
    | Some { kind = Eof; _ } | None -> None
    | Some t -> Some t
  in
  pctx.tokenizer.pos <- loc;
  pctx.tokenizer.c <- c;
  t

let rec npeek pctx n =
  let loc, c = (pctx.tokenizer.pos, pctx.tokenizer.c) in
  let t =
    match next pctx.tokenizer with
    | Some { kind = Eof; _ } | None -> []
    | Some t -> if n = 1 then [t.kind] else [t.kind] @ npeek pctx (n - 1)
  in
  pctx.tokenizer.pos <- loc;
  pctx.tokenizer.c <- c;
  t

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

let gen_id pctx : node_id = tcx_gen_id pctx.tcx

let parse_ctx_create tcx tokenizer s =
  match next tokenizer with
  | Some t ->
      {
        tcx;
        tokenizer;
        src = s;
        curr_tok = t;
        prev_tok = None;
        stop = false;
        emitter =
          {
            ctx = tcx.sess;
            source = Array.of_list (String.split_on_char '\n' s);
          };
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
    | Unit -> (
        let segment = ["unit"] in
        advance pctx;
        match pctx.curr_tok.kind with
        | Colon2 ->
            advance pctx;
            segment @ parse_path_impl ()
        | _ -> segment)
    | _ ->
        Emitter.emit pctx.emitter (unexpected_token pctx Ident pctx.curr_tok);
        exit 1
  in
  { segments = parse_path_impl (); res = Err }

let rec parse_ty pctx : ty =
  let t = pctx.curr_tok in
  match t.kind with
  | Fn ->
      advance pctx;
      ignore (eat pctx LParen);
      let arg_list = ref [] in
      let is_variadic = ref false in
      while (not pctx.stop) && pctx.curr_tok.kind <> RParen do
        match pctx.curr_tok.kind with
        | Dot3 ->
            advance pctx;
            is_variadic := true
        | _ -> (
            let ty = parse_ty pctx in
            arg_list := !arg_list @ [ty];
            match pctx.curr_tok.kind with
            | Comma -> advance pctx
            | RParen -> ()
            | _ -> assert false)
      done;
      ignore (eat pctx RParen);
      let ret_ty = Option.value ~default:Unit (parse_ret_ty pctx) in
      FnTy (!arg_list, ret_ty, !is_variadic)
  | Star ->
      advance pctx;
      Ptr (parse_ty pctx)
  | Ampersand ->
      advance pctx;
      RefTy (parse_ty pctx)
  | Ident ->
      let name = get_token_str pctx.curr_tok pctx.src in
      if Hashtbl.mem builtin_types name then (
        advance pctx;
        Hashtbl.find builtin_types name)
      else Ident (parse_path pctx)
  | _ -> unexpected_type pctx t.span

and parse_ret_ty pctx : ty option =
  match pctx.curr_tok.kind with
  | Arrow ->
      advance pctx;
      Some (parse_ty pctx)
  | _ -> None

let parse_fn_args pctx : (ty * ident * node_id) list * bool =
  assert (pctx.curr_tok.kind == LParen);
  ignore (eat pctx LParen);
  let arg_list = ref [] in
  let is_variadic = ref false in
  let first_self = ref true in
  let i = ref 0 in
  while (not pctx.stop) && pctx.curr_tok.kind <> RParen do
    (match pctx.curr_tok.kind with
    | Ampersand -> (
        advance pctx;
        let ident = parse_ident pctx in
        assert (ident = "self");
        let arg =
          (ImplicitSelf { ty = None; is_ref = true }, ident, gen_id pctx)
        in
        arg_list := !arg_list @ [arg];
        match pctx.curr_tok.kind with
        | Comma -> advance pctx
        | RParen -> ()
        | _ ->
            print_endline @@ display_span pctx.curr_tok.span;
            assert false)
    | Ident -> (
        let f ident =
          ignore (eat pctx Colon);
          let ty = parse_ty pctx in
          (ty, ident, gen_id pctx)
        in
        let arg =
          let ident = parse_ident pctx in
          match (!i, !first_self) with
          | 0, true ->
              if ident = "self" then (
                first_self := false;
                ( ImplicitSelf { ty = None; is_ref = false },
                  ident,
                  gen_id pctx ))
              else f ident
          | _, true ->
              if ident = "self" then (
                print_endline "`self` is only allowed at first position";
                assert false)
              else f ident
          | _, _ -> f ident
        in
        arg_list := !arg_list @ [arg];
        match pctx.curr_tok.kind with
        | Comma -> advance pctx
        | RParen -> ()
        | _ ->
            print_endline @@ display_span pctx.curr_tok.span;
            assert false)
    | Dot3 ->
        advance pctx;
        is_variadic := true
    | _ -> assert false);
    incr i
  done;
  ignore (eat pctx RParen);
  (!arg_list, !is_variadic)

let parse_fn_sig pctx : fn_sig =
  let s = pctx.curr_tok.span.start in
  let ident = parse_ident pctx in
  let args, is_variadic = parse_fn_args pctx in
  let ret_ty = parse_ret_ty pctx in
  { name = ident; args; ret_ty; fn_span = span s pctx; is_variadic }

let parse_pat pctx : pat =
  let kind = pctx.curr_tok.kind in
  match kind with
  | Ident -> PatIdent (get_token_str (eat pctx kind) pctx.src)
  | _ -> assert false

let rec parse_expr pctx : expr =
  strip_comments pctx;
  let expr = parse_precedence pctx 0 in
  if pctx.curr_tok.kind = As then (
    let s = pctx.curr_tok.span.start in
    advance pctx;
    {
      expr_kind = Cast (expr, parse_ty pctx);
      expr_ty = None;
      expr_id = gen_id pctx;
      expr_span = span s pctx;
    })
  else expr

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

and should_continue_as_prec_expr pctx expr =
  let is_block =
    match expr.expr_kind with If _ | Block _ -> true | _ -> false
  in
  match (is_block, pctx.curr_tok.kind, (Option.get pctx.prev_tok).kind) with
  | true, Star, RParen -> true
  | true, Star, _ ->
      let span = expr.expr_span in
      Emitter.emit pctx.emitter
        {
          level = Err;
          message = "ambiguous expression";
          span =
            {
              primary_spans = [span];
              labels =
                [(span, "try adding parenthesis around expression", true)];
            };
          children = [];
          sugg = [];
          loc = Diagnostic.dg_loc_from_span span;
        };
      false
  | false, _, _ -> true
  | _ -> true

and prec = function
  | Dot -> 80
  | Star | Slash -> 70
  | Plus | Minus -> 60
  | Ampersand -> 50
  | Pipe -> 40
  | EqEq | BangEq -> 30
  | Ampersand2 -> 20
  | Pipe2 -> 10
  | _ -> -1

and parse_prefix pctx : expr =
  let s = pctx.curr_tok.span.start in
  let expr_kind =
    match pctx.curr_tok.kind with
    | Star ->
        advance pctx;
        Deref (parse_prefix pctx)
    | Ampersand ->
        advance pctx;
        Ref (parse_prefix pctx)
    | _ -> (parse_primary pctx).expr_kind
  in
  {
    expr_kind;
    expr_ty = None;
    expr_id = gen_id pctx;
    expr_span = span s pctx;
  }

and parse_precedence pctx min_prec : expr =
  let s = pctx.curr_tok.span.start in
  let left = ref (parse_prefix pctx) in
  let p = ref (prec pctx.curr_tok.kind) in
  if should_continue_as_prec_expr pctx !left then (
    while
      (p := prec pctx.curr_tok.kind;
       !p)
      > min_prec
    do
      let kind =
        match pctx.curr_tok.kind with
        | Dot -> (
          match npeek pctx 2 with
          | [Ident; LParen] ->
              ignore (eat pctx Dot);
              let name = parse_ident pctx in
              let args = parse_call_args pctx in
              MethodCall (!left, name, args)
          | [Ident; _] ->
              ignore (eat pctx Dot);
              let field = parse_ident pctx in
              Field (!left, field)
          | _ -> assert false)
        | _ ->
            let kind = binary_kind_from_token pctx.curr_tok.kind in
            advance pctx;
            let right = parse_precedence pctx (!p + 1) in
            Binary (kind, !left, right)
      in
      left :=
        {
          expr_kind = kind;
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
    | LParen ->
        advance pctx;
        let e = parse_expr pctx in
        ignore (eat pctx RParen);
        e.expr_kind
    | _ -> parse_path_or_call pctx
  in
  {
    expr_kind;
    expr_ty = None;
    expr_id = gen_id pctx;
    expr_span = span s pctx;
  }

and parse_struct_expr pctx =
  ignore (eat pctx LBrace);
  let parse_field () : string * expr =
    let name = parse_ident pctx in
    ignore (eat pctx Colon);
    let expr = parse_expr pctx in
    (name, expr)
  in
  let rec parse_fields () : (string * expr) list =
    match pctx.curr_tok.kind with
    | RBrace -> []
    | Comma -> advance pctx; parse_fields ()
    | _ ->
        let field = [parse_field ()] in
        field @ parse_fields ()
  in
  let fields = parse_fields () in
  ignore (eat pctx RBrace);
  fields

(* parses else expr when `else` token is already eaten *)
and parse_else pctx : expr option =
  match pctx.curr_tok.kind with
  | If | LBrace -> Some (parse_expr pctx)
  | _ -> None

and parse_if pctx =
  (* if T {} *)
  (* this syntax will parse the `T {}` as a struct expr because the parser
     needs to prioritize the empty struct expression `T {}` *)
  advance pctx;
  let cond = parse_expr pctx in
  (* here we check if the condition is an empty struct expr and convert it to
     a path and empty block*)
  let cond, then_block =
    match cond.expr_kind with
    | StructExpr { struct_name; fields } when List.length fields = 0 ->
        cond.expr_kind <- Path struct_name;
        (cond, { block_stmts = []; last_expr = None; block_id = gen_id pctx })
    | _ -> (cond, parse_block pctx)
  in
  let else_block =
    if pctx.curr_tok.kind = Else then (advance pctx; parse_else pctx)
    else None
  in
  { cond; then_block; else_block }

and parse_path_or_call pctx =
  let path = parse_path pctx in
  match pctx.curr_tok.kind with
  | LParen -> Call (path, parse_call_args pctx)
  | LBrace -> (
    match npeek pctx 2 with
    | Ident :: [Colon] | RBrace :: _ ->
        StructExpr { struct_name = path; fields = parse_struct_expr pctx }
    | _ -> Path path)
  | _ -> Path path

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
  else if pctx.curr_tok.kind = Assert then (
    advance pctx;
    let expr = parse_expr pctx in
    let message =
      if pctx.curr_tok.kind = Comma then (
        advance pctx;
        Some (parse_expr pctx))
      else None
    in
    ignore (eat pctx Semi);
    Assert (expr, message))
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

let parse_type pctx : typ =
  advance pctx;
  let name = parse_ident pctx in
  let members = ref [] in
  ignore (eat pctx Eq);
  ignore (eat pctx LBrace);
  while (not pctx.stop) && pctx.curr_tok.kind <> RBrace do
    match pctx.curr_tok.kind with
    | RBrace -> ()
    | _ -> (
        let arg =
          let ident = parse_ident pctx in
          ignore (eat pctx Colon);
          let ty = parse_ty pctx in
          (ty, ident)
        in
        members := !members @ [arg];
        match pctx.curr_tok.kind with
        | Comma -> advance pctx
        | RBrace -> ()
        | _ -> assert false)
  done;
  ignore (eat pctx RBrace);
  Struct
    {
      ident = name;
      members = !members;
      struct_path = None;
      struct_id = gen_id pctx;
    }

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

let parse_impl pctx : impl =
  advance pctx;
  let impl_ty = parse_ty pctx in
  ignore (eat pctx LBrace);
  let items = ref [] in
  while pctx.curr_tok.kind <> RBrace do
    items :=
      !items
      @
      match pctx.curr_tok.kind with
      | Fn -> [AssocFn (parse_fn pctx "C" false)]
      | _ ->
          ignore (eat pctx RBrace);
          assert false
  done;
  ignore (eat pctx RBrace);
  { impl_ty; impl_items = !items }

let rec parse_item pctx : item =
  let attrs = parse_outer_attrs pctx in
  match pctx.curr_tok.kind with
  | Fn -> Fn (parse_fn pctx "C" false, attrs)
  | Type -> Type (parse_type pctx)
  | Extern -> parse_extern pctx attrs
  | Impl -> Impl (parse_impl pctx)
  | Import ->
      advance pctx;
      let import = Ast.Import (parse_path pctx) in
      ignore (eat pctx Semi);
      import
  | Unit ->
      advance pctx;
      let name = parse_ident pctx in
      ignore (eat pctx Semi);
      Unit name
  | Mod ->
      advance pctx;
      let name = parse_ident pctx in
      let modd, inline =
        pctx.curr_tok.kind
        |> function
        | Semi ->
            ignore (eat pctx Semi);
            (None, false)
        | LBrace ->
            ignore (eat pctx LBrace);
            let modd = parse_mod pctx in
            modd.mod_name <- name;
            ignore (eat pctx RBrace);
            (Some modd, true)
        | _ ->
            ignore (eat pctx LBrace);
            exit 1
      in
      let modd : item = Mod { name; resolved_mod = modd; inline } in
      modd
  | kind ->
      Printf.printf "%s\n" (display_token_kind kind);
      assert false

and parse_mod pctx : modd =
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
  while (not pctx.stop) && pctx.curr_tok.kind <> RBrace do
    strip_comments pctx;
    modd.items <- modd.items @ [parse_item pctx]
  done;
  modd
