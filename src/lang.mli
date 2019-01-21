type expression =
  | Nil
  | Number of int
  | String of string
  | Boolean of bool
  | Variable of string
  | Abstraction of string list * expression
  | NamedAbstraction of string * string list * expression list
  | Application of expression * expression list
  | IfExpression of expression * expression * expression
  | LetExpression of (string * expression) list * expression

type program = expression list

type value =
  | NilVal
  | NumVal of int
  | StrVal of string
  | BoolVal of bool
  | ConsVal of value * value
  | FuncVal of (value list -> value)

type env = (string * value) list

val _let : (string * expression) list -> expression -> expression

exception RuntimeException of string
val string_of_value : value -> string
val string_of_program : program -> string
val value_of_expression : env -> expression -> value
val eval : program -> value
