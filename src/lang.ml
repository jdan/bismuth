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

type value =
  | NilVal
  | NumVal of int
  | StrVal of string
  | BoolVal of bool
  | FuncVal of (value list -> value)

type env = (string * value) list

let string_of_value = function
  | NilVal -> "nil"
  | NumVal v -> string_of_int v
  | StrVal v -> "\"" ^ v ^ "\""
  | BoolVal v -> string_of_bool v
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

and value_env_of_expression (env: env) expr =
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

let func_of_binary_op op =
  FuncVal (function
      | [NumVal a; NumVal b] -> NumVal (op a b)
      | _ -> raise (RuntimeException "Expected exactly two ints."))

let stdlib: env = [ ("+", func_of_binary_op (+))
                  ; ("-", func_of_binary_op (-))
                  ; ("*", func_of_binary_op ( * ))
                  ; ("/", func_of_binary_op (/))
                  ; ("=",
                     (FuncVal (function
                          | [NumVal a; NumVal b] -> BoolVal (a = b)
                          | _ -> raise (RuntimeException "Expected exactly two ints."))
                     ))
                  ]

let eval exprs = match value_env_of_expressions stdlib exprs with
  | (v, _) -> v

(* Transformations *)
let rec swap_variable a b = function
  | Variable v -> Variable (if v = a then b else v)
  | IfExpression (e1, e2, e3) ->
    IfExpression ( swap_variable a b e1
                 , swap_variable a b e2
                 , swap_variable a b e3
                 )
  | Application (e1, es) ->
    Application ( swap_variable a b e1
                , List.map (fun e -> swap_variable a b e) es
                )
  | Abstraction (args, e) ->
    if List.mem a args
    then Abstraction (args, e) (* don't touch! `a` is a fresh variable *)
    else Abstraction (args, swap_variable a b e)
  | LetExpression (bindings, body) -> swap_variable a b (_let bindings body)
  | expr -> expr

let rec flatten_one = function
  | Application (e, es) as orig ->
    [orig]
    @ flatten_one e
    @ (List.map flatten_one es |> List.concat)
  | Abstraction (_, e) as orig ->
    [orig]
    @ flatten_one e
  | NamedAbstraction (_, _, es) as orig ->
    [orig]
    @ (List.map flatten_one es |> List.concat)
  | IfExpression (e1, e2, e3) as orig ->
    [orig] @ flatten_one e1 @ flatten_one e2 @ flatten_one e3
  | LetExpression (bindings, body) as orig ->
    [orig]
    @ (List.map (fun (_, e) -> flatten_one e) bindings |> List.concat)
    @ flatten_one body
  | Number v -> [Number v]
  | String v -> [String v]
  | Boolean v -> [Boolean v]
  | Variable v -> [Variable v]
  | Nil -> [Nil]
and flatten exprs = List.concat (List.map flatten_one exprs)

let num_exprs exprs = flatten exprs |> List.length

exception TraversalError
let traverse exprs pos =
  match List.nth_opt (flatten exprs) pos with
  | None -> raise TraversalError
  | Some e -> e

type replace_opts =
  { expr : expression;
    desired : expression;
    pos  : int;
  }
let replace { expr ; desired ; pos } =
  let rec inner_multi exprs n =
    (* apply inner to a sequence of exprs and return the final n *)
    let (exprs', n') = List.fold_left
        (fun (acc, n) expr ->
           let (expr', n') = inner expr n
           in (expr'::acc, n')
        )
        ([], n)
        exprs
    in (List.rev exprs', n')

  and inner expr n =
    if n < 0
    then (expr, -1) (* we could probably keep a third `done` flag around *)
    else if n = 0
    then (desired, -1)
    else match expr with
      | Application (e, es) -> (
          match inner_multi (e::es) (n - 1) with
          | (e'::es', n') -> (Application (e', es'), n')
          | _ -> raise TraversalError
        )

      | Abstraction (args, e) ->
        let (e', n') = inner e (n - 1)
        in
        (Abstraction (args, e'), n')

      | NamedAbstraction (name, args, es) -> (
          match inner_multi es (n - 1) with
          | (es', n') -> (NamedAbstraction (name, args, es'), n')
        )

      | IfExpression (e1, e2, e3) -> (
          match inner_multi [e1; e2; e3] (n - 1) with
          | ([e1'; e2'; e3'], n') -> (IfExpression (e1', e2', e3'), n')
          | _ -> raise TraversalError
        )

      | LetExpression (bindings, body) ->
        let binding_vars = List.map (fun (v, _) -> v) bindings
        and zip l1 l2 = List.map2 (fun a b -> (a, b)) l1 l2
        in let (bindings_exprs', n') = inner_multi (List.map (fun (_, e) -> e) bindings) (n - 1)
        in let (body', n'') = inner body n'

        in
        (LetExpression ((zip binding_vars bindings_exprs'), body'), n'')

      | Number v -> (Number v, n - 1)
      | String v -> (String v, n - 1)
      | Boolean v -> (Boolean v, n - 1)
      | Variable v -> (Variable v, n - 1)
      | Nil -> (Nil, n - 1)

  in match inner expr pos with
  | (expr', n) ->
    if n = -1
    then expr'
    else raise TraversalError

type abstract_opts =
  { expr : expression;
    name : string;
    pos  : int;
  }
let abstract { expr ; name ; pos } =
  let value = traverse [expr] pos
  and body = replace { expr = expr;
                       pos = pos;
                       desired = Variable name;
                     }
  in LetExpression ([(name, value)], body)
