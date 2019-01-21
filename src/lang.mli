type expression =
    Nil
  | Number of int
  | String of string
  | Boolean of bool
  | Variable of string
  | Abstraction of string list * expression
  | NamedAbstraction of string * string list * expression list
  | Application of expression * expression list
  | IfExpression of expression * expression * expression
  | LetExpression of (string * expression) list * expression

type value =
    NilVal
  | NumVal of int
  | StrVal of string
  | BoolVal of bool
  | FuncVal of (value list -> value)
type env = (string * value) list

val _let : (string * expression) list -> expression -> expression

exception RuntimeException of string
val string_of_expression : expression -> string
val string_of_program : expression list -> string
val value_of_expression : env -> expression -> value
val eval : expression list -> value
