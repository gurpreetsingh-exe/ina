open Ast
open Ty
open Token
open Tokenizer
open Errors
open Diagnostic
open Session
open Source
open Span

let builtin_types =
  [|
    ("i8", Ty.Int I8);
    ("i16", Int I16);
    ("i32", Int I32);
    ("i64", Int I64);
    ("isize", Int Isize);
    ("u8", Int U8);
    ("u16", Int U16);
    ("u32", Int U32);
    ("u64", Int U64);
    ("usize", Int Usize);
    ("f32", Float F32);
    ("f64", Float F64);
    ("bool", Bool);
    ("str", Str);
  |]
  |> Array.to_seq |> Hashtbl.of_seq

type parse_ctx = {
  tcx : tcx;
  tokenizer : tokenizer;
  src : string;
  mutable curr_tok : token;
  mutable prev_tok : token option;
  mutable stop : bool;
}

let span (start : int) (pcx : parse_ctx) =
  match pcx.prev_tok with
  | Some t -> Span.{ lo = start; hi = t.span.hi }
  | None -> assert false

let advance pcx =
  match next pcx.tokenizer with
  | Some { kind = Eof; _ } -> pcx.stop <- true
  | Some t ->
      pcx.prev_tok <- Some pcx.curr_tok;
      pcx.curr_tok <- t
  | None -> assert false

let peek pcx =
  let loc, c = (pcx.tokenizer.pos, pcx.tokenizer.c) in
  let t =
    match next pcx.tokenizer with
    | Some { kind = Eof; _ } | None -> None
    | Some t -> Some t
  in
  pcx.tokenizer.pos <- loc;
  pcx.tokenizer.c <- c;
  t

let rec npeek pcx n =
  let loc, c = (pcx.tokenizer.pos, pcx.tokenizer.c) in
  let t =
    match next pcx.tokenizer with
    | Some { kind = Eof; _ } | None -> []
    | Some t -> if n = 1 then [t.kind] else [t.kind] @ npeek pcx (n - 1)
  in
  pcx.tokenizer.pos <- loc;
  pcx.tokenizer.c <- c;
  t

let unexpected_token pcx expected t =
  let msg =
    Printf.sprintf "expected `%s`, found `%s`"
      (display_token_kind expected)
      (display_token_kind t.kind)
  in
  let span = pcx.curr_tok.span in
  {
    level = Err;
    message = "unexpected token";
    span = { primary_spans = [span]; labels = [(span, msg, true)] };
    children = [];
    sugg = [];
    loc = Diagnostic.loc __POS__;
  }

let unexpected_type pcx span =
  Sess.emit_err pcx.tcx.sess
    {
      level = Err;
      message = "unexpected type";
      span = { primary_spans = [span]; labels = [] };
      children = [];
      sugg = [];
      loc = Diagnostic.loc __POS__;
    };
  exit 1

let eat pcx kind =
  if pcx.curr_tok.kind == kind then (
    let t = pcx.curr_tok in
    advance pcx; t)
  else (
    Sess.emit_err pcx.tcx.sess (unexpected_token pcx kind pcx.curr_tok);
    exit 1)

let gen_id pcx : node_id = tcx_gen_id pcx.tcx

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
      }
  | None -> exit 0

let strip_comments pcx =
  let rec impl = function
    | Comment None -> advance pcx; impl pcx.curr_tok.kind
    | _ -> ()
  in
  impl pcx.curr_tok.kind

let parse_ident pcx = get_token_str (eat pcx Ident) pcx.src

let parse_attr pcx : normal_attr =
  ignore (eat pcx LBracket);
  let ident = parse_ident pcx in
  ignore (eat pcx RBracket);
  { name = ident }

let parse_outer_attrs pcx : attr list =
  let unexpected_inner_attr () =
    Sess.emit_err pcx.tcx.sess
      {
        level = Err;
        message = "unexpected inner attribute after outer attribute";
        span = { primary_spans = [pcx.curr_tok.span]; labels = [] };
        children = [];
        sugg = [];
        loc = Diagnostic.loc __POS__;
      };
    exit 1
  in
  let rec parse_outer_attrs_impl () =
    let attr_kind =
      match pcx.curr_tok.kind with
      | Bang -> unexpected_inner_attr ()
      | LBracket -> Some (NormalAttr (parse_attr pcx))
      | Comment style -> (
        match style with
        | Some Outer ->
            let outer = get_token_str pcx.curr_tok pcx.src in
            advance pcx; Some (Doc outer)
        | Some Inner -> unexpected_inner_attr ()
        | None -> advance pcx; None)
      | _ -> None
    in
    match attr_kind with
    | Some kind -> [{ kind; style = Outer }] @ parse_outer_attrs_impl ()
    | None -> []
  in
  parse_outer_attrs_impl ()

let parse_inner_attrs pcx : attr list =
  let rec parse_inner_attrs_impl () =
    let attr_kind =
      match pcx.curr_tok.kind with
      | Bang ->
          advance pcx;
          Some (NormalAttr (parse_attr pcx))
      | Comment style -> (
        match style with
        | Some Inner ->
            let inner = get_token_str pcx.curr_tok pcx.src in
            advance pcx; Some (Doc inner)
        | Some Outer -> None
        | None -> advance pcx; None)
      | _ -> None
    in
    match attr_kind with
    | Some kind -> [{ kind; style = Inner }] @ parse_inner_attrs_impl ()
    | None -> []
  in
  parse_inner_attrs_impl ()

let parse_path pcx : path =
  let rec parse_path_impl () =
    match pcx.curr_tok.kind with
    | Ident -> (
        let segment = [parse_ident pcx] in
        match pcx.curr_tok.kind with
        | Colon2 ->
            advance pcx;
            segment @ parse_path_impl ()
        | _ -> segment)
    | Unit -> (
        let segment = ["unit"] in
        advance pcx;
        match pcx.curr_tok.kind with
        | Colon2 ->
            advance pcx;
            segment @ parse_path_impl ()
        | _ -> segment)
    | _ ->
        Sess.emit_err pcx.tcx.sess (unexpected_token pcx Ident pcx.curr_tok);
        exit 1
  in
  { segments = parse_path_impl (); res = Err }

let rec parse_ty pcx : ty =
  let t = pcx.curr_tok in
  match t.kind with
  | Fn ->
      advance pcx;
      ignore (eat pcx LParen);
      let arg_list = ref [] in
      let is_variadic = ref false in
      while (not pcx.stop) && pcx.curr_tok.kind <> RParen do
        match pcx.curr_tok.kind with
        | Dot3 ->
            advance pcx;
            is_variadic := true
        | _ -> (
            let ty = parse_ty pcx in
            arg_list := !arg_list @ [ty];
            match pcx.curr_tok.kind with
            | Comma -> advance pcx
            | RParen -> ()
            | _ -> assert false)
      done;
      ignore (eat pcx RParen);
      let ret_ty = Option.value ~default:Unit (parse_ret_ty pcx) in
      FnTy (!arg_list, ret_ty, !is_variadic)
  | Star ->
      advance pcx;
      Ptr (parse_ty pcx)
  | Ampersand ->
      advance pcx;
      RefTy (parse_ty pcx)
  | Ident ->
      let name = get_token_str pcx.curr_tok pcx.src in
      if Hashtbl.mem builtin_types name then (
        advance pcx;
        Hashtbl.find builtin_types name)
      else Ident (parse_path pcx)
  | _ -> unexpected_type pcx t.span

and parse_ret_ty pcx : ty option =
  match pcx.curr_tok.kind with
  | Arrow ->
      advance pcx;
      Some (parse_ty pcx)
  | _ -> None

let parse_fn_args pcx : (ty * ident * node_id) list * bool =
  assert (pcx.curr_tok.kind == LParen);
  ignore (eat pcx LParen);
  let arg_list = ref [] in
  let is_variadic = ref false in
  let first_self = ref true in
  let i = ref 0 in
  while (not pcx.stop) && pcx.curr_tok.kind <> RParen do
    (match pcx.curr_tok.kind with
    | Ampersand -> (
        advance pcx;
        let ident = parse_ident pcx in
        assert (ident = "self");
        let arg =
          (ImplicitSelf { ty = None; is_ref = true }, ident, gen_id pcx)
        in
        arg_list := !arg_list @ [arg];
        match pcx.curr_tok.kind with
        | Comma -> advance pcx
        | RParen -> ()
        | _ ->
            print_endline @@ display_span pcx.curr_tok.span;
            assert false)
    | Ident -> (
        let f ident =
          ignore (eat pcx Colon);
          let ty = parse_ty pcx in
          (ty, ident, gen_id pcx)
        in
        let arg =
          let ident = parse_ident pcx in
          match (!i, !first_self) with
          | 0, true ->
              if ident = "self" then (
                first_self := false;
                ( ImplicitSelf { ty = None; is_ref = false },
                  ident,
                  gen_id pcx ))
              else f ident
          | _, true ->
              if ident = "self" then (
                print_endline "`self` is only allowed at first position";
                assert false)
              else f ident
          | _, _ -> f ident
        in
        arg_list := !arg_list @ [arg];
        match pcx.curr_tok.kind with
        | Comma -> advance pcx
        | RParen -> ()
        | _ ->
            print_endline @@ display_span pcx.curr_tok.span;
            assert false)
    | Dot3 ->
        advance pcx;
        is_variadic := true
    | _ -> assert false);
    incr i
  done;
  ignore (eat pcx RParen);
  (!arg_list, !is_variadic)

let parse_fn_sig pcx : fn_sig =
  let s = pcx.curr_tok.span.lo in
  let ident = parse_ident pcx in
  let args, is_variadic = parse_fn_args pcx in
  let ret_ty = parse_ret_ty pcx in
  { name = ident; args; ret_ty; fn_span = span s pcx; is_variadic }

let parse_pat pcx : pat =
  let kind = pcx.curr_tok.kind in
  match kind with
  | Ident -> PatIdent (get_token_str (eat pcx kind) pcx.src)
  | _ -> assert false

let rec parse_expr pcx : expr =
  strip_comments pcx;
  let expr = parse_precedence pcx 0 in
  if pcx.curr_tok.kind = As then (
    let s = pcx.curr_tok.span.lo in
    advance pcx;
    {
      expr_kind = Cast (expr, parse_ty pcx);
      expr_ty = None;
      expr_id = gen_id pcx;
      expr_span = span s pcx;
    })
  else expr

and parse_call_args pcx : expr list =
  let args = ref [] in
  ignore (eat pcx LParen);
  while (not pcx.stop) && pcx.curr_tok.kind <> RParen do
    args := !args @ [parse_expr pcx];
    match pcx.curr_tok.kind with
    | Comma -> advance pcx
    | RParen -> ()
    | _ ->
        Sess.emit_err pcx.tcx.sess (unexpected_token pcx RParen pcx.curr_tok)
  done;
  ignore (eat pcx RParen);
  !args

and should_continue_as_prec_expr pcx expr =
  let is_block =
    match expr.expr_kind with If _ | Block _ -> true | _ -> false
  in
  match (is_block, pcx.curr_tok.kind, (Option.get pcx.prev_tok).kind) with
  | true, Star, RParen -> true
  | true, Star, _ ->
      let span = expr.expr_span in
      Sess.emit_err pcx.tcx.sess
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
          loc = Diagnostic.loc __POS__;
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
  | LAngle | RAngle | EqEq | BangEq -> 30
  | Ampersand2 -> 20
  | Pipe2 -> 10
  | _ -> -1

and parse_prefix pcx : expr =
  let s = pcx.curr_tok.span.lo in
  let expr_kind =
    match pcx.curr_tok.kind with
    | Star ->
        advance pcx;
        Deref (parse_prefix pcx)
    | Ampersand ->
        advance pcx;
        Ref (parse_prefix pcx)
    | _ -> (parse_primary pcx).expr_kind
  in
  { expr_kind; expr_ty = None; expr_id = gen_id pcx; expr_span = span s pcx }

and parse_precedence pcx min_prec : expr =
  let s = pcx.curr_tok.span.lo in
  let left = ref (parse_prefix pcx) in
  let p = ref (prec pcx.curr_tok.kind) in
  if should_continue_as_prec_expr pcx !left then (
    while
      (p := prec pcx.curr_tok.kind;
       !p)
      > min_prec
    do
      let kind =
        match pcx.curr_tok.kind with
        | Dot -> (
          match npeek pcx 2 with
          | [Ident; LParen] ->
              ignore (eat pcx Dot);
              let name = parse_ident pcx in
              let args = parse_call_args pcx in
              MethodCall (!left, name, args)
          | [Ident; _] ->
              ignore (eat pcx Dot);
              let field = parse_ident pcx in
              Field (!left, field)
          | _ -> assert false)
        | _ ->
            let kind = binary_kind_from_token pcx.curr_tok.kind in
            advance pcx;
            let right = parse_precedence pcx (!p + 1) in
            Binary (kind, !left, right)
      in
      left :=
        {
          expr_kind = kind;
          expr_ty = None;
          expr_id = gen_id pcx;
          expr_span = span s pcx;
        }
    done;
    !left)
  else !left

and parse_primary pcx : expr =
  let s = pcx.curr_tok.span.lo in
  let expr_kind =
    match pcx.curr_tok.kind with
    | Lit lit as kind ->
        let buf = get_token_str (eat pcx kind) pcx.src in
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
    | If -> If (parse_if pcx)
    | LBrace -> Block (parse_block pcx)
    | LParen ->
        advance pcx;
        let e = parse_expr pcx in
        ignore (eat pcx RParen);
        e.expr_kind
    | _ -> parse_path_or_call pcx
  in
  { expr_kind; expr_ty = None; expr_id = gen_id pcx; expr_span = span s pcx }

and parse_struct_expr pcx =
  ignore (eat pcx LBrace);
  let parse_field () : string * expr =
    let name = parse_ident pcx in
    ignore (eat pcx Colon);
    let expr = parse_expr pcx in
    (name, expr)
  in
  let rec parse_fields () : (string * expr) list =
    match pcx.curr_tok.kind with
    | RBrace -> []
    | Comma -> advance pcx; parse_fields ()
    | _ ->
        let field = [parse_field ()] in
        field @ parse_fields ()
  in
  let fields = parse_fields () in
  ignore (eat pcx RBrace);
  fields

(* parses else expr when `else` token is already eaten *)
and parse_else pcx : expr option =
  match pcx.curr_tok.kind with
  | If | LBrace -> Some (parse_expr pcx)
  | _ -> None

and parse_if pcx =
  (* if T {} *)
  (* this syntax will parse the `T {}` as a struct expr because the parser
     needs to prioritize the empty struct expression `T {}` *)
  advance pcx;
  let cond = parse_expr pcx in
  (* here we check if the condition is an empty struct expr and convert it to
     a path and empty block*)
  let cond, then_block =
    match cond.expr_kind with
    | StructExpr { struct_name; fields } when List.length fields = 0 ->
        cond.expr_kind <- Path struct_name;
        (cond, { block_stmts = []; last_expr = None; block_id = gen_id pcx })
    | _ -> (cond, parse_block pcx)
  in
  let else_block =
    if pcx.curr_tok.kind = Else then (advance pcx; parse_else pcx) else None
  in
  { cond; then_block; else_block }

and parse_path_or_call pcx =
  let path = parse_path pcx in
  match pcx.curr_tok.kind with
  | LParen -> Call (path, parse_call_args pcx)
  | LBrace -> (
    match npeek pcx 2 with
    | Ident :: [Colon] | RBrace :: _ ->
        StructExpr { struct_name = path; fields = parse_struct_expr pcx }
    | _ -> Path path)
  | _ -> Path path

and parse_let pcx : binding =
  ignore (eat pcx Let);
  let binding_create pat ty =
    ignore (eat pcx Eq);
    let binding_expr = parse_expr pcx in
    ignore (eat pcx Semi);
    {
      binding_pat = pat;
      binding_ty = ty;
      binding_expr;
      binding_id = gen_id pcx;
    }
  in
  let pat = parse_pat pcx in
  match pcx.curr_tok.kind with
  | Eq -> binding_create pat None
  | Colon ->
      advance pcx;
      binding_create pat (Some (parse_ty pcx))
  | _ -> assert false

and parse_stmt pcx : stmt =
  if pcx.curr_tok.kind = Let then Binding (parse_let pcx)
  else if pcx.curr_tok.kind = Assert then (
    advance pcx;
    let expr = parse_expr pcx in
    let message =
      if pcx.curr_tok.kind = Comma then (
        advance pcx;
        Some (parse_expr pcx))
      else None
    in
    ignore (eat pcx Semi);
    Assert (expr, message))
  else (
    let expr = parse_expr pcx in
    match pcx.curr_tok.kind with
    | Semi -> advance pcx; Stmt expr
    | Eq ->
        advance pcx;
        let init = parse_expr pcx in
        ignore (eat pcx Semi);
        Assign (expr, init)
    | _ -> Expr expr)

and parse_block pcx : block =
  let stmt_list = ref [] in
  let last_expr = ref None in
  ignore (eat pcx LBrace);
  while (not pcx.stop) && pcx.curr_tok.kind <> RBrace do
    match pcx.curr_tok.kind with
    | RBrace -> ()
    | _ -> (
        let stmt = parse_stmt pcx in
        (match !last_expr with
        | Some expr ->
            stmt_list := !stmt_list @ [Stmt expr];
            last_expr := None
        | None -> ());
        match stmt with
        | Expr expr -> last_expr := Some expr
        | stmt -> stmt_list := !stmt_list @ [stmt])
  done;
  ignore (eat pcx RBrace);
  { block_stmts = !stmt_list; last_expr = !last_expr; block_id = gen_id pcx }

let parse_fn pcx abi is_extern : func =
  advance pcx;
  let sign = parse_fn_sig pcx in
  if pcx.curr_tok.kind == Semi then (
    advance pcx;
    {
      is_extern;
      abi;
      fn_sig = sign;
      body = None;
      func_id = gen_id pcx;
      func_path = None;
    })
  else (
    let body = parse_block pcx in
    {
      is_extern;
      abi;
      fn_sig = sign;
      body = Some body;
      func_id = gen_id pcx;
      func_path = None;
    })

let parse_type pcx : typ =
  advance pcx;
  let name = parse_ident pcx in
  let members = ref [] in
  ignore (eat pcx Eq);
  ignore (eat pcx LBrace);
  while (not pcx.stop) && pcx.curr_tok.kind <> RBrace do
    match pcx.curr_tok.kind with
    | RBrace -> ()
    | _ -> (
        let arg =
          let ident = parse_ident pcx in
          ignore (eat pcx Colon);
          let ty = parse_ty pcx in
          (ty, ident)
        in
        members := !members @ [arg];
        match pcx.curr_tok.kind with
        | Comma -> advance pcx
        | RBrace -> ()
        | _ -> assert false)
  done;
  ignore (eat pcx RBrace);
  Struct { ident = name; members = !members; struct_id = gen_id pcx }

let parse_extern pcx attrs : item =
  advance pcx;
  let abi =
    match pcx.curr_tok.kind with
    | Lit String ->
        let buf = get_token_str (eat pcx pcx.curr_tok.kind) pcx.src in
        let s = String.sub buf 1 (String.length buf - 2) in
        let s = Scanf.unescaped s in
        s
    | _ -> "C"
  in
  match pcx.curr_tok.kind with
  | LBrace ->
      let f () =
        advance pcx;
        let items = ref [] in
        while pcx.curr_tok.kind <> RBrace do
          items := !items @ [parse_fn pcx abi true]
        done;
        ignore (eat pcx RBrace);
        !items
      in
      Foreign (f ())
  | Fn -> Fn (parse_fn pcx abi true, attrs)
  | _ -> assert false

let parse_impl pcx : impl =
  advance pcx;
  let impl_ty = parse_ty pcx in
  ignore (eat pcx LBrace);
  let items = ref [] in
  while pcx.curr_tok.kind <> RBrace do
    items :=
      !items
      @
      match pcx.curr_tok.kind with
      | Fn -> [AssocFn (parse_fn pcx "C" false)]
      | _ ->
          ignore (eat pcx RBrace);
          assert false
  done;
  ignore (eat pcx RBrace);
  { impl_ty; impl_items = !items }

let rec parse_item pcx : item =
  let attrs = parse_outer_attrs pcx in
  match pcx.curr_tok.kind with
  | Fn -> Fn (parse_fn pcx "C" false, attrs)
  | Type -> Type (parse_type pcx)
  | Extern -> parse_extern pcx attrs
  | Impl -> Impl (parse_impl pcx)
  | Import ->
      advance pcx;
      let import = Ast.Import (parse_path pcx) in
      ignore (eat pcx Semi);
      import
  | Unit ->
      advance pcx;
      let name = parse_ident pcx in
      ignore (eat pcx Semi);
      Unit name
  | Mod ->
      advance pcx;
      let name = parse_ident pcx in
      let modd, inline =
        pcx.curr_tok.kind
        |> function
        | Semi ->
            ignore (eat pcx Semi);
            (None, false)
        | LBrace ->
            ignore (eat pcx LBrace);
            let modd = parse_mod pcx in
            modd.mod_name <- name;
            ignore (eat pcx RBrace);
            (Some modd, true)
        | _ ->
            ignore (eat pcx LBrace);
            exit 1
      in
      let modd : item = Mod { name; resolved_mod = modd; inline } in
      modd
  | kind ->
      Printf.printf "%s\n" (display_token_kind kind);
      assert false

and parse_mod pcx : modd =
  let mod_attrs = parse_inner_attrs pcx in
  let mod_path = pcx.tokenizer.filename in
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
      mod_id = gen_id pcx;
    }
  in
  while (not pcx.stop) && pcx.curr_tok.kind <> RBrace do
    strip_comments pcx;
    modd.items <- modd.items @ [parse_item pcx]
  done;
  modd

let parse_mod_from_file tcx path =
  let file = Source_map.load_file tcx.sess.source_map path in
  let tokenizer = Tokenizer.tokenize path file.src in
  let pcx = parse_ctx_create tcx tokenizer file.src in
  parse_mod pcx
