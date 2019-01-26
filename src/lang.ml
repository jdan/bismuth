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

let rec string_of_value = function
  | NilVal -> "nil"
  | NumVal v -> string_of_int v
  | StrVal v -> "\"" ^ v ^ "\""
  | BoolVal v -> string_of_bool v
  | ConsVal (a, b) -> "(" ^ string_of_value a ^ " . " ^ string_of_value b ^ ")"
  | FuncVal _ -> "#func"

(* let x = e in f <=> ((fn (x) f) e) *)
let rec _let bindings body =
  let (names, values) = List.fold_left
      (fun (names', values') (name', value') ->
         ( name'::names'
         , value'::values'
         ))
      ([], [])
      bindings
  in
  Application ( Abstraction (names, body)
              , values
              )

exception RuntimeException of string
let rec value_env_of_expressions (env: env) =
  List.fold_left
    (fun (_, env') -> value_env_of_expression env')
    (NilVal, env)

and value_of_expression env expr = match value_env_of_expression env expr with
  | (v, _) -> v

and value_env_of_expression env expr =
  let with_env v = (v, env)
  in match expr with
  | Nil -> NilVal |> with_env
  | Number v -> NumVal v |> with_env
  | String v -> StrVal v |> with_env
  | Boolean v -> BoolVal v |> with_env
  | Variable v -> (try List.assoc v env with Not_found ->
      raise (RuntimeException ("Unbound variable: " ^ v))
    ) |> with_env
  | Abstraction (args, e) ->
    FuncVal (fun in_args ->
        let env' = List.map2 (fun a b -> (a, b)) args in_args @ env
        in value_of_expression env' e
      ) |> with_env
  | NamedAbstraction (name, args, es) ->
    let rec func = FuncVal (fun in_args ->
        let env' = List.map2 (fun a b -> (a, b)) args in_args @ (name, func) :: env
        in match value_env_of_expressions env' es with
        | (v, _) -> v
      )
    in (func, (name, func) :: env)
  | Application (e1, es) -> (
      match value_env_of_expression env e1 with
      | (FuncVal f, _) ->
        f (List.map (value_of_expression env) es)
      | (v, _) -> raise (RuntimeException (
          "Expected function, received " ^ string_of_value v ^
          ". Did you apply a function too many times?"))
    ) |> with_env
  | IfExpression (e1, e2, e3) -> (
      match value_env_of_expression env e1 with
      | (BoolVal true, _) -> value_env_of_expression env e2
      | (BoolVal false, _) -> value_env_of_expression env e3
      | (v, _) -> raise (
          RuntimeException ("Expected boolean, received " ^ string_of_value v)
        )
    )
  (* _let and _apply are basically transformations *)
  | LetExpression (bindings, body) ->
    _let bindings body |> value_env_of_expression env

let rec string_of_expression = function
  | Nil -> "nil"
  | Number v -> string_of_int v
  | String v -> "\"" ^ v ^ "\""
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Variable v -> v
  | Abstraction (args, e) ->
    "(fn (" ^
    String.concat " " args ^
    ") " ^
    string_of_expression e ^
    ")"
  | NamedAbstraction (name, args, es) ->
    "(fun (" ^
    name ^ " " ^
    String.concat " " args ^
    ") " ^
    String.concat " " (List.map string_of_expression es) ^
    ")"
  | Application (e1, es) ->
    "(" ^
    string_of_expression e1 ^ " " ^
    String.concat " " (List.map string_of_expression es) ^
    ")"
  | IfExpression (e1, e2, e3) ->
    "(if " ^
    String.concat " " (List.map string_of_expression [e1; e2; e3]) ^
    ")"
  | LetExpression (bindings, body) ->
    let string_of_binding (b, v) = "(" ^ b ^ " " ^ string_of_expression v ^ ")"
    in
    "(let [" ^
    String.concat " " (List.map string_of_binding bindings) ^
    "] " ^
    string_of_expression body ^
    ")"
and string_of_program exprs =
  String.concat "\n" (List.map string_of_expression exprs)

module Stdlib : sig
  val all: env
end = struct
  let func_of_binary_op op =
    FuncVal (function
        | [NumVal a; NumVal b] -> op a b
        | _ -> raise (RuntimeException "Expected exactly two ints."))

  let arith =
    [ ("+", func_of_binary_op (fun a b -> NumVal (a + b)))
    ; ("-", func_of_binary_op (fun a b -> NumVal (a - b)))
    ; ("*", func_of_binary_op (fun a b -> NumVal (a * b)))
    ; ("/", func_of_binary_op (fun a b -> NumVal (a / b)))
    ]

  let list =
    let car = function
      | [ConsVal (a, _)] -> a
      | _ -> raise (RuntimeException "Expected a single cons.")
    and cdr = function
      | [ConsVal (_, b)] -> b
      | _ -> raise (RuntimeException "Expected a single cons.")
    in
    [ ("cons", FuncVal (function
          | [a; b] -> ConsVal (a, b)
          | _ -> raise (RuntimeException "Expected two values.")))
    ; ("list", FuncVal (
          let rec list = function
            | [] -> NilVal
            | x::xs -> ConsVal (x, list xs)
          in list ))
    ; ("car", FuncVal car)
    ; ("cdr", FuncVal cdr)
    ; ("caar", FuncVal (fun ls -> car [car ls]))
    ; ("cadr", FuncVal (fun ls -> car [cdr ls]))
    ; ("cdar", FuncVal (fun ls -> cdr [car ls]))
    ; ("cddr", FuncVal (fun ls -> cdr [cdr ls]))
    ; ("caaar", FuncVal (fun ls -> car [car [car ls]]))
    ; ("caadr", FuncVal (fun ls -> car [car [cdr ls]]))
    ; ("cadar", FuncVal (fun ls -> car [cdr [car ls]]))
    ; ("caddr", FuncVal (fun ls -> car [cdr [cdr ls]]))
    ; ("cdaar", FuncVal (fun ls -> cdr [car [car ls]]))
    ; ("cdadr", FuncVal (fun ls -> cdr [car [cdr ls]]))
    ; ("cddar", FuncVal (fun ls -> cdr [cdr [car ls]]))
    ; ("cdddr", FuncVal (fun ls -> cdr [cdr [cdr ls]]))
    ]

  let all =
    arith @
    list @
    [ ("=", FuncVal (function
          | [a; b] -> BoolVal (a = b)
          | _ -> raise (RuntimeException "Expected exactly two values.")))
    ]
end

let eval exprs = match value_env_of_expressions Stdlib.all exprs with
  | (v, _) -> v
