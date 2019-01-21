open Lang

val swap_variable : string -> string -> expression -> expression
exception TraversalError
val flatten : expression list -> expression list
val num_exprs : expression list -> int
val traverse : expression list -> int -> expression
type replace_opts = { expr : expression; desired : expression; pos : int; }
val replace : replace_opts -> expression
type abstract_opts = { expr : expression; name : string; pos : int; }
val abstract : abstract_opts -> expression
