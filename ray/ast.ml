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
  mod_id : node_id;
}

and item =
  | Fn of func * attr_list
  | Const of constant
  | Import of import

and fn_sig = {
  name : ident;
  args : (ty * ident option) list;
  ret_ty : ty option;
}

and func = {
  is_extern : bool;
  fn_sig : fn_sig;
  body : block option;
  func_id : node_id;
}

and block = {
  block_stmts : stmt list;
  last_expr : expr option;
  block_id : node_id;
}

and stmt =
  | Stmt of expr
  | Binding of binding

and binding = {
  binding_pat : pat;
  binding_ty : ty option;
  binding_expr : expr;
  binding_id : node_id;
}

and pat = Ident of ident

and constant = {
  const_name : ident;
  const_ty : ty;
  const_expr : expr;
  const_id : node_id;
}

and import = ident

and ty =
  | Prim of prim_ty
  | FnTy of (ty list * ty)
  | Unit

and prim_ty =
  | I32
  | I64
  | Bool

and expr = Lit of lit

and lit =
  | LitInt of int
  | LitBool of bool
