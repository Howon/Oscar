type operator = Add | Sub | Mul | Div
type expr =
    | Lit of int
    | Binop of expr * operator * expr
    | Seq of expr * expr
    | Asn of int * expr
    | Var of int
