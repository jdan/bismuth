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

exception RuntimeException of string
val string_of_expression : expression -> string
val string_of_program : expression list -> string
val value_of_expression : env -> expression -> value
val eval : expression list -> value
val swap_variable : string -> string -> expression -> expression

exception TraversalError
val flatten : expression list -> expression list
val num_exprs : expression list -> int
val traverse : expression list -> int -> expression
type replace_opts = { expr : expression; desired : expression; pos : int; }
val replace : replace_opts -> expression
type abstract_opts = { expr : expression; name : string; pos : int; }
val abstract : abstract_opts -> expression
