open Lang

val swap_variable : string -> string -> expression -> expression
exception TraversalError
val flatten : program -> expression list
val num_exprs : program -> int
val traverse : program -> int -> expression
type replace_opts = { expr : expression; desired : expression; pos : int; }
val replace : replace_opts -> expression
type abstract_opts = { expr : expression; name : string; pos : int; }
val abstract : abstract_opts -> expression
val pretty_string_of_program : int -> program -> string

