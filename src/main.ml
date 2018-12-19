type expression =
    (* Values *)
    | Nil
    | Number of int
    | String of string
    | Boolean of bool

    (* Other stuff *)
    | Variable of string
    | Abstraction of string * expression
    | Application of expression * expression

type value =
    | NilVal
    | NumVal of int
    | StrVal of string
    | BoolVal of bool
    | FuncVal of (value -> value)

type env = (string * value) list

let string_of_value = function
    | NilVal -> "nil"
    | NumVal v -> string_of_int v
    | StrVal v -> v
    | BoolVal v -> string_of_bool v
    | FuncVal _ -> "#func"

exception RuntimeException of string
let rec value_of_expression env = function
    | Nil -> NilVal
    | Number v -> NumVal v
    | String v -> StrVal v
    | Boolean v -> BoolVal v
    | Variable v -> (try List.assoc v env with Not_found ->
        raise (RuntimeException ("Unbound variable: " ^ v))
    )
    | Abstraction (p, e) ->
        FuncVal (fun new_p -> value_of_expression ((p, new_p) :: env) e)
    | Application (e1, e2) -> (
        match value_of_expression env e1 with
        | FuncVal f -> f (value_of_expression env e2)
        | v -> raise (RuntimeException ("Expected function, received " ^ string_of_value v))
    )
