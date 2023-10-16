open Token
open Source
open Structures
open Structures.Vec

type ident = string
type path_segment = { ident: ident }

type path = {
    segments: path_segment vec
  ; span: Span.t
}

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

type ty =
  | Int of int_ty
  | Float of float_ty
  | Bool
  | Str
  | Ptr of ty
  | Ref of ty
  | FnTy of ty vec * ty * bool
  | Path of path
  | ImplicitSelf
  | Unit
  | CVarArgs
  | Err

type attr = {
    kind: attr_kind
  ; style: attr_style
  ; attr_span: Span.t
}

and attr_vec = attr vec

and attr_kind =
  | Doc of string
  | NormalAttr of normal_attr

and normal_attr = { name: ident }

and attr_style =
  | Outer
  | Inner

type modd = {
    mutable items: item vec
  ; mutable attrs: attr vec
  ; mutable mod_name: string
  ; mod_path: string
  ; mod_span: Span.t
}

and strukt = {
    ident: string
  ; mutable members: (ty * string) vec
  ; struct_span: Span.t
}

and typ = Struct of strukt
and assoc_item = AssocFn of func

and impl = {
    impl_ty: ty
  ; impl_items: assoc_item vec
  ; impl_span: Span.t
}

and item =
  | Fn of func * attr_vec
  | Type of typ
  | Foreign of func vec
  | Impl of impl
  | Mod of {
        name: string
      ; mutable resolved_mod: modd option
      ; inline: bool
    }
  | Unit of string

and fn_sig = {
    name: ident
  ; mutable args: (ty * ident) vec
  ; ret_ty: ty option
  ; fn_span: Span.t
  ; is_variadic: bool
}

and func = {
    is_extern: bool
  ; abi: string
  ; fn_sig: fn_sig
  ; body: block option
  ; mutable func_path: path option
  ; func_span: Span.t
}

and block = {
    block_stmts: stmt vec
  ; last_expr: expr option
  ; block_span: Span.t
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
}

and pat = PatIdent of ident

and expr = {
    mutable expr_kind: expr_kind
  ; expr_span: Span.t
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
  | Ref of expr
  | StructExpr of struct_expr
  | Field of expr * ident
  | Cast of expr * ty
  | MethodCall of expr * string * expr vec

and struct_expr = {
    struct_name: path
  ; fields: (string * expr) vec
  ; struct_expr_span: Span.t
}

and iff = {
    cond: expr
  ; then_block: block
  ; else_block: expr option
  ; if_span: Span.t
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
