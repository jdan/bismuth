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

  (* Special forms *)
  | IfExpression of expression * expression * expression
  | LetExpression of (string * expression) list * expression
  | MultiApplication of expression * expression list

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
  | StrVal v -> "\"" ^ v ^ "\""
  | BoolVal v -> string_of_bool v
  | FuncVal _ -> "#func"

let _apply f args =
  (* Apply a list of expressions to a curried function *)
  let rec inner acc = function
    | [] -> acc
    | arg::args -> inner (Application (acc, arg)) args
  in inner f args

let rec _let bindings body =
  (* let x = e in f <=> ((fn (x) f) e) *)
  let single binding exp body =
    Application (( Abstraction (binding, body)
                 , exp
                 ))
  in match bindings with
  | [] -> body
  | (v, e)::bindings' ->
    single v e (
      _let bindings' body
    )

exception RuntimeException of string
let rec value_of_expression (env: env) = function
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
      | v -> raise (RuntimeException (
          "Expected function, received " ^ string_of_value v ^
          ". Did you apply a function too many times?"))
    )
  | IfExpression (e1, e2, e3) -> (
      match value_of_expression env e1 with
      | BoolVal true -> value_of_expression env e2
      | BoolVal false -> value_of_expression env e3
      | v -> raise (
          RuntimeException ("Expected boolean, received " ^ string_of_value v)
        )
    )
  (* _let and _apply are basically transformations *)
  | LetExpression (bindings, body) -> _let bindings body |> value_of_expression env
  | MultiApplication (f, args) -> _apply f args |> value_of_expression env

let rec string_of_expression = function
  | Nil -> "nil"
  | Number v -> string_of_int v
  | String v -> "\"" ^ v ^ "\""
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Variable v -> v
  | Abstraction (p, e) -> "(fn (" ^ p ^ ") " ^ string_of_expression e ^ ")"
  | Application (e1, e2) -> "(" ^ string_of_expression e1 ^ " " ^ string_of_expression e2 ^ ")"
  | IfExpression (e1, e2, e3) -> "(if " ^ String.concat " " (List.map string_of_expression [e1; e2; e3]) ^ ")"
  | LetExpression (bindings, body) ->
    let string_of_binding (b, v) = "(" ^ b ^ " " ^ string_of_expression v ^ ")"
    in
    "(let [" ^
    String.concat " " (List.map string_of_binding bindings) ^
    "] " ^
    string_of_expression body ^
    ")"
  | MultiApplication (f, args) ->
    "(" ^
    string_of_expression f ^
    " " ^
    String.concat " " (List.map string_of_expression args) ^
    ")"

let func_of_binary_op op =
  let error_message a b =
    RuntimeException (
      "Expected (int, int), received " ^
      (string_of_value a) ^ ", " ^ (string_of_value b)
    )
  in
  FuncVal (fun a -> FuncVal (fun b ->
      match (a, b) with
      | (NumVal a', NumVal b') -> NumVal (op a' b')
      | (u, v) -> raise (error_message u v)))

let stdlib: env = [ ("+", func_of_binary_op (+))
                  ; ("-", func_of_binary_op (-))
                  ; ("*", func_of_binary_op ( * ))
                  ; ("/", func_of_binary_op (/))
                  ]

let eval = value_of_expression stdlib

(* Transformations *)
let rec swap_variable a b = function
  | Variable v -> Variable (if v = a then b else v)
  | IfExpression (e1, e2, e3) ->
    IfExpression ( swap_variable a b e1
                 , swap_variable a b e2
                 , swap_variable a b e3
                 )
  | Application (e1, e2) ->
    Application ( swap_variable a b e1
                , swap_variable a b e2
                )
  | Abstraction (v, e) ->
    if v = a
    then Abstraction (a, e) (* don't touch! `a` is a fresh variable *)
    else Abstraction (v, swap_variable a b e)
  | LetExpression (bindings, body) -> swap_variable a b (_let bindings body)
  | MultiApplication (f, args) -> swap_variable a b (_apply f args)
  | expr -> expr

let rec flatten = function
  | Application (e1, e2) as orig ->
    [orig] 
    @ flatten e1 
    @ flatten e2
  | Abstraction (_, e) as orig ->
    [orig] 
    @ flatten e
  | IfExpression (e1, e2, e3) as orig ->
    [orig] @ flatten e1 @ flatten e2 @ flatten e3
  | LetExpression (bindings, body) as orig ->
    [orig] 
    @ (List.map (fun (_, e) -> flatten e) bindings |> List.concat)
    @ flatten body
  | MultiApplication (e, es) as orig ->
    [orig] 
    @ flatten e
    @ (List.map flatten es |> List.concat)
  | atom -> [atom] 

let num_exprs expr = flatten expr |> List.length

exception TraversalError
let traverse expr pos =
  match List.nth_opt (flatten expr) pos with
  | None -> raise TraversalError
  | Some e -> e