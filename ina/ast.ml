open Token
open Source
open Structures.Vec

type node_id = int
type ident = string

type int_ty =
  | I8
  | I16
  | I32
  | I64
  | Isize
  | U8
  | U16
  | U32
  | U64
  | Usize

type float_ty =
  | F64
  | F32

type ty_kind =
  | Int of int_ty
  | Float of float_ty
  | Bool
  | Str
  | Ptr of (mutability * ty)
  | Ref of (mutability * ty)
  | Slice of ty
  | FnPtr of ty vec * ty * bool
  | Path of path
  | ImplicitSelf
  | Unit
  | CVarArgs
  | Err

and mutability =
  | Mut
  | Imm

and path_segment = {
    ident: ident
  ; args: ty vec option
  ; span: Span.t
  ; id: node_id
}

and path = {
    segments: path_segment vec
  ; span: Span.t
  ; path_id: node_id
}

and ty = {
    kind: ty_kind
  ; span: Span.t
  ; ty_id: node_id
}

let mk_ty kind span ty_id = { kind; span; ty_id }
let is_self ty = match ty.kind with ImplicitSelf -> true | _ -> false

type attr = {
    kind: attr_kind
  ; style: attr_style
  ; attr_span: Span.t
  ; attr_id: node_id
}

and attr_vec = attr vec

and attr_kind =
  | Doc of string
  | NormalAttr of normal_attr

and normal_attr = { name: ident }

and attr_style =
  | Outer
  | Inner

let is_test attrs =
  find
    (function
     | { kind = NormalAttr { name = "test" }; _ } as attr -> Some attr
     | _ -> None)
    attrs
  |> Option.is_some
;;

type modd = {
    mutable items: item vec
  ; mutable attrs: attr vec
  ; mutable mod_name: string
  ; mod_path: string
  ; mod_span: Span.t
  ; mod_id: node_id
}

and generic_param_kind = Ident of ident

and generic_param = {
    kind: generic_param_kind
  ; span: Span.t
  ; id: node_id
}

and generics = {
    params: generic_param vec
  ; span: Span.t
  ; id: node_id
}

and strukt = {
    name: string
  ; mutable fields: (ty * string) vec
  ; generics: generics
  ; span: Span.t
  ; id: node_id
}

and variant = {
    name: string
  ; fields: ty vec
  ; span: Span.t
  ; id: node_id
}

and adt = {
    name: string
  ; variants: variant vec
  ; generics: generics
  ; span: Span.t
  ; id: node_id
}

and typ =
  | Struct of strukt
  | Adt of adt

and assoc_item = AssocFn of func

and impl = {
    ty: ty
  ; generics: generics
  ; items: assoc_item vec
  ; span: Span.t
  ; id: node_id
}

and using_kind =
  | Simple of ident option
  | Val of ident * Span.t
  | Nested of using vec
  | Glob

and using = {
    prefix: path
  ; kind: using_kind
  ; span: Span.t
  ; id: node_id
}

and item =
  | Fn of func * attr_vec
  | Type of typ
  | Foreign of func vec * node_id
  | Impl of impl
  | Mod of {
        name: string
      ; mutable resolved_mod: modd option
      ; inline: bool
      ; span: Span.t
    }
  | ExternMod of string
  | Using of using

and arg = {
    ty: ty
  ; arg: ident
  ; arg_id: node_id
}

and fn_sig = {
    mutable args: arg vec
  ; ret_ty: ty option
  ; fn_span: Span.t
  ; is_variadic: bool
  ; fn_sig_id: node_id
}

and func = {
    is_extern: bool
  ; name: ident
  ; abi: string
  ; fn_sig: fn_sig
  ; fn_generics: generics
  ; body: block option
  ; func_span: Span.t
  ; func_id: node_id
}

and block = {
    block_stmts: stmt vec
  ; last_expr: expr option
  ; block_span: Span.t
  ; block_id: node_id
}

and stmt =
  | Stmt of expr
  | Expr of expr
  | Assign of expr * expr
  | Binding of binding
  | Assert of expr * expr option

and binding = {
    binding_pat: pat
  ; mutable binding_ty: ty option
  ; binding_expr: expr
  ; binding_id: node_id
  ; binding_span: Span.t
}

and pat =
  | PIdent of (mutability * ident * node_id ref)
  | PPath of path
  | PCons of (path * pat vec)
  | POr of pat vec
  | PRange of (lit * lit)
  | PLit of lit
  | PWild

and expr = {
    mutable expr_kind: expr_kind
  ; expr_span: Span.t
  ; expr_id: node_id
}

and binary_kind =
  | Add
  | Sub
  | Mul
  | Div
  | Gt
  | Lt
  | Eq
  | NotEq
  | GtEq
  | LtEq
  | BitAnd
  | BitOr
  | And
  | Or

and expr_kind =
  | Lit of lit
  | Path of path
  | Call of expr * expr vec
  | Binary of binary_kind * expr * expr
  | If of iff
  | Block of block
  | Deref of expr
  | Ref of (mutability * expr)
  | StructExpr of struct_expr
  | Field of expr * ident
  | Cast of expr * ty
  | MethodCall of expr * path_segment * expr vec
  | Match of expr * arm vec
  | Slice of expr vec
  | Index of (expr * expr)
  | Hole

and arm = {
    pat: pat
  ; patspan: Span.t
  ; expr: expr
  ; span: Span.t
}

and struct_expr = {
    struct_name: path
  ; fields: (string * expr) vec
  ; struct_expr_span: Span.t
  ; struct_expr_id: node_id
}

and iff = {
    cond: expr
  ; then_block: block
  ; else_block: expr option
  ; if_span: Span.t
  ; if_id: node_id
}

and lit =
  | LitInt of int
  | LitFloat of float
  | LitBool of bool
  | LitStr of string

let binary_kind_from_token = function
  | Plus -> Add
  | Minus -> Sub
  | Star -> Mul
  | Slash -> Div
  | EqEq -> Eq
  | BangEq -> NotEq
  | Ampersand -> BitAnd
  | Pipe -> BitOr
  | Ampersand2 -> And
  | Pipe2 -> Or
  | LAngle -> Lt
  | RAngle -> Gt
  | _ -> assert false
;;
