type ident = string

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
}

and block = {
  block_stmts : stmt list;
  last_expr : expr option;
}

and stmt =
  | Stmt of expr
  | Binding of binding

and binding = {
  binding_pat : pat;
  binding_ty : ty option;
  binding_expr : expr;
}

and pat = Ident of ident

and constant = {
  const_name : ident;
  const_ty : ty;
  const_expr : expr;
}

and import = ident

and ty =
  | Int
  | Bool

and expr = Lit of lit

and lit =
  | LitInt of int
  | LitBool of bool
