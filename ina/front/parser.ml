open Ast
open Token
open Tokenizer
open Errors
open Diagnostic
open Handler
open Source
open Source_map
open Span
open Structures.Vec
open Printf

type 'a presult = ('a, diagnostic) result

let ( let* ) res f = match res with Ok v -> f v | Error e -> Error e

let builtin_types =
  [|
     "i8", Ast.Int I8
   ; "i16", Int I16
   ; "i32", Int I32
   ; "i64", Int I64
   ; "isize", Int Isize
   ; "u8", Int U8
   ; "u16", Int U16
   ; "u32", Int U32
   ; "u64", Int U64
   ; "usize", Int Usize
   ; "f32", Float F32
   ; "f64", Float F64
   ; "bool", Bool
   ; "str", Str
  |]
  |> Array.to_seq
  |> Hashtbl.of_seq
;;

type parse_sess = {
    sm: source_map
  ; span_diagnostic: handler
  ; mutable node_id: node_id
}

let prec = function
  | Dot -> 80
  | Star | Slash -> 70
  | Plus | Minus -> 60
  | Ampersand -> 50
  | Pipe -> 40
  | LAngle | RAngle | EqEq | BangEq -> 30
  | Ampersand2 -> 20
  | Pipe2 -> 10
  | _ -> -1
;;

let parse_spanned_with_sep
    parser
    (beginn : token_kind)
    (endd : token_kind)
    (sep : token_kind)
    (f : unit -> 'a presult)
    : 'a vec presult
  =
  let* _ = parser#expect beginn in
  let items = new vec in
  let first = ref true in
  let body () =
    let* item = f () in
    items#push item;
    Ok ()
  in
  let rec parse' () =
    match parser#check endd with
    | true -> Ok ()
    | _ when parser#check sep && not !first ->
        parser#bump;
        Ok ()
    | false ->
        if !first then first := false;
        let* _ = body () in
        (match parser#check sep with
         | true ->
             parser#bump;
             parse' ()
         | false -> Ok ())
  in
  let* _ = parse' () in
  assert (parser#eat endd);
  Ok items
;;

class parser pcx file tokenizer =
  object (self)
    val pcx : parse_sess = pcx
    val file : file = file
    val mutable token : token = { kind = Eof; span = Span.make 0 0 }
    val mutable prev_token : token option = None
    val tokenizer : tokenizer = tokenizer
    val expected_tokens : token_kind vec = new vec
    method token = token

    method check kind =
      let is_present = token.kind = kind in
      if not is_present then expected_tokens#push kind;
      is_present

    method eat kind =
      let is_present = self#check kind in
      if is_present then self#bump;
      is_present

    method eat_if_present kind = if self#check kind then self#bump

    method npeek n =
      let loc, c = tokenizer.pos, tokenizer.c in
      let t =
        match next tokenizer with
        | Some { kind = Eof; _ } | None -> []
        | Some t -> if n = 1 then [t.kind] else [t.kind] @ self#npeek (n - 1)
      in
      tokenizer.pos <- loc;
      tokenizer.c <- c;
      t

    method unexpected_try_recover kind =
      let msg =
        sprintf
          "expected `%s` found `%s`"
          (display_token_kind kind)
          (display_token_kind token.kind)
      in
      let e = self#err token.span msg in
      self#bump;
      Error e

    method err span msg =
      let messages = new vec in
      messages#push { msg; style = NoStyle };
      new diagnostic Err messages (multi_span span)

    method emit_err e = pcx.span_diagnostic#emit_diagnostic e

    method expect_one_of edible inedible =
      if List.mem token.kind edible
      then (
        self#bump;
        Ok false)
      else if List.mem token.kind inedible
      then Ok false
      else Error (self#err token.span "unexpected_token")

    method expect kind =
      if expected_tokens#empty
      then
        if token.kind = kind
        then (
          self#bump;
          Ok false)
        else self#unexpected_try_recover kind
      else self#expect_one_of [kind] []

    method bump =
      (match next tokenizer with
       | Some t ->
           prev_token <- Some token;
           token <- t
       | None -> ());
      expected_tokens#clear

    method mk_span start =
      match prev_token with
      | Some t -> Span.make start t.span.hi
      | None -> assert false

    method id =
      let i = pcx.node_id in
      pcx.node_id <- pcx.node_id + 1;
      i

    method parse_ident =
      if token.kind = Ident
      then (
        let ident = get_token_str token file#src in
        self#bump;
        Ok ident)
      else Error (self#err token.span "expected ident")

    method parse_attr =
      let* _ = self#expect LBracket in
      let* ident = self#parse_ident in
      let* _ = self#expect RBracket in
      Ok { name = ident }

    method parse_inner_attrs =
      let s = token.span.lo in
      let attrs = new vec in
      let rec parse_inner_attrs_impl () =
        let* kind =
          match token.kind with
          | Bang ->
              self#bump;
              let* attr = self#parse_attr in
              Ok [NormalAttr attr]
          | Comment style ->
              (match style with
               | Some Inner ->
                   let inner = get_token_str token file#src in
                   self#bump;
                   Ok [Doc inner]
               | Some Outer -> Ok []
               | None ->
                   self#bump;
                   Ok [])
          | _ -> Ok []
        in
        match kind with
        | [kind] ->
            attrs#push
              {
                kind
              ; style = Inner
              ; attr_span = self#mk_span s
              ; attr_id = self#id
              };
            let* _ = parse_inner_attrs_impl () in
            Ok ()
        | _ -> Ok ()
      in
      let* _ = parse_inner_attrs_impl () in
      Ok attrs

    method parse_outer_attrs =
      let err = self#err token.span "unexpected inner attribute" in
      let s = token.span.lo in
      let attrs = new vec in
      let rec parse_outer_attrs_impl () =
        let* kind =
          match token.kind with
          | Bang ->
              pcx.span_diagnostic#emit_diagnostic err;
              (* recover and parse it as outer attribute *)
              self#bump;
              let* attr = self#parse_attr in
              Ok (Some [NormalAttr attr])
          | LBracket ->
              let* attr = self#parse_attr in
              Ok (Some [NormalAttr attr])
          | Comment style ->
              (match style with
               | Some Outer ->
                   let outer = get_token_str token file#src in
                   self#bump;
                   Ok (Some [Doc outer])
               | Some Inner ->
                   pcx.span_diagnostic#emit_diagnostic err;
                   (* recover and parse it as outer doc comment *)
                   let outer = get_token_str token file#src in
                   self#bump;
                   Ok (Some [Doc outer])
               | None ->
                   self#bump;
                   Ok (Some []))
          | _ -> Ok None
        in
        match kind with
        | Some [kind] ->
            attrs#push
              {
                kind
              ; style = Outer
              ; attr_span = self#mk_span s
              ; attr_id = self#id
              };
            let* _ = parse_outer_attrs_impl () in
            Ok ()
        | Some [] -> parse_outer_attrs_impl ()
        | None -> Ok ()
        | _ -> Ok ()
      in
      let* _ = parse_outer_attrs_impl () in
      Ok attrs

    method unexpected_token ?(line = __LINE__) kind =
      let msg =
        sprintf "%d: unexpected token `%s`" line @@ display_token_kind kind
      in
      self#emit_err (self#err token.span msg);
      exit 1

    method parse_ty =
      let s = token.span.lo in
      match token.kind with
      | Fn ->
          let var_arg = ref false in
          self#bump;
          let* args =
            parse_spanned_with_sep self LParen RParen Comma (fun () ->
                if token.kind = Dot3 then var_arg := true;
                self#parse_ty)
          in
          let unit_ty : ty =
            { kind = Unit; span = { lo = 0; hi = 0 }; ty_id = self#id }
          in
          let* ret_ty = self#parse_ret_ty in
          let ret_ty = Option.value ~default:unit_ty ret_ty in
          Ok
            (mk_ty (FnPtr (args, ret_ty, !var_arg)) (self#mk_span s) self#id)
      | Ident ->
          let name = get_token_str token file#src in
          (match Hashtbl.find_opt builtin_types name with
           | Some ty ->
               self#bump;
               Ok (mk_ty ty (self#mk_span s) self#id)
           | None ->
               let* path = self#parse_path in
               let ty : ty_kind = Path path in
               Ok (mk_ty ty (self#mk_span s) self#id))
      | Star ->
          self#bump;
          let* ty = self#parse_ty in
          Ok (mk_ty (Ptr ty) (self#mk_span s) self#id)
      | Ampersand ->
          self#bump;
          let* ty = self#parse_ty in
          Ok (mk_ty (Ref ty) (self#mk_span s) self#id)
      | Dot3 ->
          self#bump;
          Ok (mk_ty CVarArgs (self#mk_span s) self#id)
      | t ->
          self#unexpected_token t ~line:__LINE__;
          exit 1

    method parse_fn_args =
      let i = ref 0 in
      let var_arg = ref false in
      let parse_arg () =
        let* arg = self#parse_ident in
        let _ = self#eat Colon in
        let* ty = self#parse_ty in
        Ok { ty; arg; arg_id = self#id }
      in
      let rec maybe_parse_self () =
        match token.kind with
        | Ampersand ->
            let s = token.span.lo in
            self#bump;
            let* self_ = maybe_parse_self () in
            (match self_ with
             | { ty; arg = "self"; _ } when is_self ty ->
                 let ty : ty_kind = Ref ty in
                 Ok
                   {
                     ty = mk_ty ty (self#mk_span s) self#id
                   ; arg = "self"
                   ; arg_id = self#id
                   }
             | _ -> assert false)
        | Ident ->
            let s = token.span.lo in
            let* arg = self#parse_ident in
            if arg = "self"
            then
              Ok
                {
                  ty = mk_ty ImplicitSelf (self#mk_span s) self#id
                ; arg
                ; arg_id = self#id
                }
            else
              let* _ = self#expect Colon in
              let* ty = self#parse_ty in
              Ok { ty; arg; arg_id = self#id }
        | t ->
            self#unexpected_token t ~line:__LINE__;
            exit 1
      in
      let parse_arg' () =
        if !var_arg
        then self#emit_err (self#err token.span "param after variadic-arg");
        if self#check Dot3
        then (
          let s = token.span.lo in
          self#bump;
          incr i;
          var_arg := true;
          Ok
            {
              ty = mk_ty CVarArgs (self#mk_span s) self#id
            ; arg = ""
            ; arg_id = self#id
            })
        else
          let arg = if !i = 0 then maybe_parse_self () else parse_arg () in
          incr i;
          arg
      in
      let* args =
        parse_spanned_with_sep self LParen RParen Comma parse_arg'
      in
      Ok (args, !var_arg)

    method parse_ret_ty =
      match token.kind with
      | Arrow ->
          self#bump;
          let* ty = self#parse_ty in
          Ok (Some ty)
      | _ -> Ok None

    method parse_fn_sig =
      let s = token.span.lo in
      let* ident = self#parse_ident in
      let* args, is_variadic = self#parse_fn_args in
      let* ret_ty = self#parse_ret_ty in
      Ok
        {
          name = ident
        ; args
        ; ret_ty
        ; fn_span = self#mk_span s
        ; is_variadic
        ; fn_sig_id = self#id
        }

    method parse_pat =
      let kind = token.kind in
      match kind with
      | Ident ->
          let* ident = self#parse_ident in
          Ok (PatIdent ident)
      | t ->
          self#unexpected_token t ~line:__LINE__;
          exit 1

    method parse_let =
      let s = token.span.lo in
      let* _ = self#expect Let in
      let binding_create pat ty =
        let* _ = self#expect Eq in
        let* binding_expr = self#parse_expr in
        let* _ = self#expect Semi in
        Ok
          {
            binding_pat = pat
          ; binding_ty = ty
          ; binding_expr
          ; binding_id = self#id
          ; binding_span = self#mk_span s
          }
      in
      let* pat = self#parse_pat in
      match token.kind with
      | Eq -> binding_create pat None
      | Colon ->
          self#bump;
          let* ty = self#parse_ty in
          binding_create pat (Some ty)
      | t ->
          self#unexpected_token t ~line:__LINE__;
          exit 1

    method parse_stmt =
      let* _ = self#parse_outer_attrs in
      match token.kind with
      | Let ->
          let* binding = self#parse_let in
          Ok (Binding binding)
      | Assert ->
          self#bump;
          let* expr = self#parse_expr in
          let* message =
            if token.kind = Comma
            then (
              self#bump;
              let* expr = self#parse_expr in
              Ok (Some expr))
            else Ok None
          in
          let* _ = self#expect Semi in
          Ok (Ast.Assert (expr, message))
      | _ ->
          let* expr = self#parse_expr in
          (match token.kind with
           | Semi ->
               self#bump;
               Ok (Stmt expr)
           | Eq ->
               self#bump;
               let* init = self#parse_expr in
               let* _ = self#expect Semi in
               Ok (Assign (expr, init))
           | _ -> Ok (Expr expr))

    method should_continue_as_prec_expr expr =
      let is_block =
        match expr.expr_kind with If _ | Block _ -> true | _ -> false
      in
      match is_block, token.kind, (Option.get prev_token).kind with
      | true, Star, RParen -> true
      | true, Star, _ -> false
      | false, _, _ -> true
      | _ -> true

    method parse_primary =
      let s = token.span.lo in
      let* expr_kind =
        match token.kind with
        | Lit lit ->
            let buf = get_token_str token file#src in
            self#bump;
            Ok
              (Ast.Lit
                 (match lit with
                  | Int -> LitInt (int_of_string buf)
                  | Float -> LitFloat (float_of_string buf)
                  | Bool -> LitBool (bool_of_string buf)
                  | String ->
                      let s = String.sub buf 1 (String.length buf - 2) in
                      let s = Scanf.unescaped s in
                      LitStr s
                  | lit_kind ->
                      ignore
                        (Printf.printf "%s\n" (display_literal lit_kind));
                      assert false))
        | If ->
            let* iff = self#parse_if in
            Ok (Ast.If iff)
        | LBrace ->
            let* block = self#parse_block in
            Ok (Block block)
        | LParen ->
            self#bump;
            let* e = self#parse_expr in
            let* _ = self#expect RParen in
            Ok e.expr_kind
        | _ -> self#parse_path_or_call
      in
      Ok { expr_kind; expr_span = self#mk_span s; expr_id = self#id }

    method parse_struct_expr =
      let parse_field () =
        let* name = self#parse_ident in
        let* _ = self#expect Colon in
        let* expr = self#parse_expr in
        Ok (name, expr)
      in
      parse_spanned_with_sep self LBrace RBrace Comma parse_field

    (* parses else expr when `else` token is already eaten *)
    method parse_else =
      match token.kind with
      | If | LBrace ->
          let* expr = self#parse_expr in
          Ok (Some expr)
      | _ -> Ok None

    method parse_if =
      (* if T {} *)
      (* this syntax will parse the `T {}` as a struct expr because the parser
         needs to prioritize the empty struct expression `T {}` *)
      let s = token.span.lo in
      self#bump;
      let* cond = self#parse_expr in
      (* here we check if the condition is an empty struct expr and convert it to
         a path and empty block*)
      let* cond, then_block =
        match cond.expr_kind with
        | StructExpr { struct_name; fields; _ } when fields#empty ->
            cond.expr_kind <- Path struct_name;
            Ok
              ( cond
              , {
                  block_stmts = new vec
                ; last_expr = None
                ; block_span = cond.expr_span
                ; block_id = self#id
                } )
        | _ ->
            let* block = self#parse_block in
            Ok (cond, block)
      in
      let* else_block =
        if token.kind = Else
        then (
          self#bump;
          let* elze = self#parse_else in
          Ok elze)
        else Ok None
      in
      Ok
        {
          cond
        ; then_block
        ; else_block
        ; if_span = self#mk_span s
        ; if_id = self#id
        }

    method parse_path =
      let s = token.span.lo in
      let segments = new vec in
      let rec parse_path_impl () =
        match token.kind with
        | Ident ->
            let s = token.span.lo in
            let* ident = self#parse_ident in
            let segment = { ident; span = self#mk_span s } in
            segments#push segment;
            (match token.kind with
             | Colon2 ->
                 self#bump;
                 let* _ = parse_path_impl () in
                 Ok ()
             | _ -> Ok ())
        | Unit ->
            let segment = { ident = "unit"; span = self#mk_span s } in
            segments#push segment;
            self#bump;
            (match token.kind with
             | Colon2 ->
                 self#bump;
                 let* _ = parse_path_impl () in
                 Ok ()
             | _ -> Ok ())
        | _ ->
            (* Sess.emit_err pcx.tcx.sess (unexpected_token pcx Ident token); *)
            exit 1
      in
      let* _ = parse_path_impl () in
      Ok { segments; span = self#mk_span s; path_id = self#id }

    method parse_call_args =
      parse_spanned_with_sep self LParen RParen Comma (fun () ->
          self#parse_expr)

    method parse_path_or_call =
      let s = token.span.lo in
      let* path = self#parse_path in
      match token.kind with
      | LParen ->
          let* args = self#parse_call_args in
          let path =
            {
              expr_kind = Path path
            ; expr_span = self#mk_span s
            ; expr_id = self#id
            }
          in
          Ok (Call (path, args))
      | LBrace ->
          (match self#npeek 2 with
           | Ident :: [Colon] | RBrace :: _ ->
               let* fields = self#parse_struct_expr in
               Ok
                 (StructExpr
                    {
                      struct_name = path
                    ; fields
                    ; struct_expr_span = self#mk_span s
                    ; struct_expr_id = self#id
                    })
           | _ -> Ok (Path path))
      | _ -> Ok (Path path)

    method parse_prefix =
      let s = token.span.lo in
      let* expr_kind =
        match token.kind with
        | Star ->
            self#bump;
            let* expr = self#parse_prefix in
            Ok (Deref expr)
        | Ampersand ->
            self#bump;
            let* expr = self#parse_prefix in
            Ok (Ref expr)
        | _ ->
            let* expr = self#parse_primary in
            Ok expr.expr_kind
      in
      Ok { expr_kind; expr_span = self#mk_span s; expr_id = self#id }

    method parse_precedence min_prec =
      let s = token.span.lo in
      let* left = self#parse_prefix in
      let left = ref left in
      let p = ref (prec token.kind) in
      if self#should_continue_as_prec_expr !left
      then (
        while
          (p := prec token.kind;
           !p)
          > min_prec
        do
          let f () =
            let* kind =
              match token.kind with
              | Dot ->
                  (match self#npeek 2 with
                   | [Ident; LParen] ->
                       let* _ = self#expect Dot in
                       let* name = self#parse_ident in
                       let* args = self#parse_call_args in
                       Ok (MethodCall (!left, name, args))
                   | [Ident; _] ->
                       let* _ = self#expect Dot in
                       let* field = self#parse_ident in
                       Ok (Field (!left, field))
                   | t ->
                       self#unexpected_token (List.hd t) ~line:__LINE__;
                       exit 1)
              | _ ->
                  let kind = binary_kind_from_token token.kind in
                  self#bump;
                  let* right = self#parse_precedence (!p + 1) in
                  Ok (Binary (kind, !left, right))
            in
            left :=
              {
                expr_kind = kind
              ; expr_span = self#mk_span s
              ; expr_id = self#id
              };
            Ok ()
          in
          ignore (f ())
        done;
        Ok !left)
      else Ok !left

    method parse_expr =
      let* expr = self#parse_precedence 0 in
      if token.kind = As
      then (
        let s = token.span.lo in
        self#bump;
        let* ty = self#parse_ty in
        Ok
          {
            expr_kind = Cast (expr, ty)
          ; expr_span = self#mk_span s
          ; expr_id = self#id
          })
      else Ok expr

    method parse_block =
      let s = token.span.lo in
      let stmts = new vec in
      let last_expr = ref None in
      if self#eat LBrace
      then (
        while token.kind <> RBrace do
          match token.kind with
          | RBrace -> ()
          | _ ->
              let f () =
                let* stmt = self#parse_stmt in
                (match !last_expr with
                 | Some expr ->
                     stmts#push (Stmt expr);
                     last_expr := None
                 | None -> ());
                match stmt with
                | Expr expr ->
                    last_expr := Some expr;
                    Ok ()
                | stmt ->
                    stmts#push stmt;
                    Ok ()
              in
              (match f () with Ok () -> () | Error e -> self#emit_err e)
        done;
        ignore (self#eat RBrace);
        Ok
          {
            block_stmts = stmts
          ; last_expr = !last_expr
          ; block_span = self#mk_span s
          ; block_id = self#id
          })
      else Error (self#err token.span "expected `{`")

    method parse_fn abi is_extern =
      let s = token.span.lo in
      self#bump;
      let* sign = self#parse_fn_sig in
      if token.kind == Semi
      then (
        self#bump;
        Ok
          {
            is_extern
          ; abi
          ; fn_sig = sign
          ; body = None
          ; func_path = None
          ; func_span = self#mk_span s
          ; func_id = self#id
          })
      else
        let* body = self#parse_block in
        Ok
          {
            is_extern
          ; abi
          ; fn_sig = sign
          ; body = Some body
          ; func_path = None
          ; func_span = self#mk_span s
          ; func_id = self#id
          }

    method parse_type =
      let s = token.span.lo in
      self#bump;
      let* name = self#parse_ident in
      let* _ = self#expect Eq in
      let parse_field () =
        let* ident = self#parse_ident in
        let* _ = self#expect Colon in
        let* ty = self#parse_ty in
        Ok (ty, ident)
      in
      let* fields =
        parse_spanned_with_sep self LBrace RBrace Comma parse_field
      in
      Ok
        (Struct
           {
             ident = name
           ; members = fields
           ; struct_span = self#mk_span s
           ; struct_id = self#id
           })

    method parse_impl =
      let s = token.span.lo in
      self#bump;
      let* impl_ty = self#parse_ty in
      let* _ = self#expect LBrace in
      let items = new vec in
      while token.kind <> RBrace do
        let f () =
          let* item =
            match token.kind with
            | Fn ->
                let* fn = self#parse_fn "default" false in
                Ok (AssocFn fn)
            | t ->
                self#unexpected_token ?line:(Some __LINE__) t;
                exit 1
          in
          items#push item;
          Ok ()
        in
        ignore (f ())
      done;
      let* _ = self#expect RBrace in
      Ok
        {
          impl_ty
        ; impl_items = items
        ; impl_span = self#mk_span s
        ; impl_id = self#id
        }

    method parse_extern attrs =
      self#bump;
      let abi =
        if self#check (Lit String)
        then (
          let buf = get_token_str token file#src in
          self#bump;
          let s = String.sub buf 1 (String.length buf - 2) in
          let s = Scanf.unescaped s in
          s)
        else "C"
      in
      match token.kind with
      | LBrace ->
          let f () =
            self#bump;
            let items = new vec in
            while not @@ self#check RBrace do
              match self#parse_fn abi true with
              | Ok fn -> items#push fn
              | Error e -> self#emit_err e
            done;
            let* _ = self#expect RBrace in
            Ok items
          in
          let* items = f () in
          Ok (Foreign items)
      | Fn ->
          let* fn = self#parse_fn abi true in
          Ok (Ast.Fn (fn, attrs))
      | _ -> assert false

    method parse_item =
      let* attrs = self#parse_outer_attrs in
      match token.kind with
      | Fn ->
          let* fn = self#parse_fn "default" false in
          Ok (Ast.Fn (fn, attrs))
      | Type ->
          let* ty = self#parse_type in
          Ok (Ast.Type ty)
      | Extern ->
          let* extern_item = self#parse_extern attrs in
          Ok extern_item
      | Impl ->
          let* impl = self#parse_impl in
          Ok (Ast.Impl impl)
      | Unit ->
          self#bump;
          let* name = self#parse_ident in
          let* _ = self#expect Semi in
          Ok (Ast.Unit name)
      | Mod ->
          self#bump;
          let* name = self#parse_ident in
          let* modd, inline =
            token.kind |> function
            | Semi ->
                self#bump;
                Ok (None, false)
            | LBrace ->
                self#bump;
                let* modd = self#parse_mod in
                modd.mod_name <- name;
                let* _ = self#expect RBrace in
                Ok (Some modd, true)
            | _ -> Error (self#err token.span "unexpected_token")
          in
          let modd : item = Mod { name; resolved_mod = modd; inline } in
          Ok modd
      | t ->
          self#unexpected_token t ~line:__LINE__;
          exit 1

    method parse_mod =
      let s = token.span.lo in
      let* mod_attrs = self#parse_inner_attrs in
      let mod_path = tokenizer.filename in
      let mod_name =
        match Filename.remove_extension (Filename.basename mod_path) with
        | "lib" -> Filename.basename (Filename.dirname mod_path)
        | other -> other
      in
      let items = new vec in
      while not (self#check Eof || self#check RBrace) do
        let f () =
          match self#parse_item with
          | Ok item -> items#push item
          | Error e -> pcx.span_diagnostic#emit_diagnostic e
        in
        f ()
      done;
      Ok
        {
          items
        ; attrs = mod_attrs
        ; mod_name
        ; mod_path
        ; mod_span = self#mk_span s
        ; mod_id = self#id
        }
  end

let parse_mod_from_file pcx path =
  let file = pcx.sm#load_file path in
  let tokenizer = Tokenizer.tokenize path file#src in
  let parser = new parser pcx file tokenizer in
  parser#bump;
  parser#parse_mod
;;
