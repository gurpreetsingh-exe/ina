open Token

type ident = string

type node_id = int

type attr = {
  kind : attr_kind;
  style : attr_style;
}

and attr_list = attr list

and attr_kind =
  | Doc of string
  | NormalAttr of normal_attr

and normal_attr = { name : ident }

and attr_style =
  | Outer
  | Inner

type modd = {
  mutable items : item list;
  mutable attrs : attr list;
  imported_mods : (string, modd) Hashtbl.t;
  mod_name : string;
  mod_path : string;
  mod_id : node_id;
}

and item =
  | Fn of func * attr_list
  | Const of constant
  | Import of path

and fn_sig = {
  name : ident;
  args : (ty * ident) list;
  ret_ty : ty option;
  fn_span : span;
  is_variadic : bool;
}

and func = {
  is_extern : bool;
  abi : string;
  fn_sig : fn_sig;
  body : block option;
  func_id : node_id;
  mutable func_path : path option;
}

and block = {
  block_stmts : stmt list;
  last_expr : expr option;
  block_id : node_id;
}

and stmt =
  | Stmt of expr
  | Expr of expr
  | Assign of expr * expr
  | Binding of binding

and binding = {
  binding_pat : pat;
  mutable binding_ty : ty option;
  binding_expr : expr;
  binding_id : node_id;
}

and pat = PatIdent of ident

and constant = {
  const_name : ident;
  const_ty : ty;
  const_expr : expr;
  const_id : node_id;
}

and path_segment = ident

and path = { mutable segments : path_segment list }

and ty =
  | Prim of prim_ty
  | Ptr of ty
  | RefTy of ty
  | FnTy of (ty list * ty * bool)
  | Unit

and prim_ty =
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
  | F64
  | F32
  | Bool
  | Str

and expr = {
  expr_kind : expr_kind;
  mutable expr_ty : ty option;
  expr_id : node_id;
  expr_span : span;
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

and expr_kind =
  | Lit of lit
  | Path of path
  | Call of path * expr list
  | Binary of binary_kind * expr * expr
  | Deref of expr
  | Ref of expr

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
  | _ -> assert false

type lang_item =
  | Mod of modd
  | Fn of func
