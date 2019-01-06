type expression =
  (* Values *)
  | Nil
  | Number of int
  | String of string
  | Boolean of bool

  (* Other stuff *)
  | Variable of string
  | Abstraction of string list * expression
  | Application of expression * expression list

  (* Special forms *)
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
let rec value_of_expression (env: env) = function
  | Nil -> NilVal
  | Number v -> NumVal v
  | String v -> StrVal v
  | Boolean v -> BoolVal v
  | Variable v -> (try List.assoc v env with Not_found ->
      raise (RuntimeException ("Unbound variable: " ^ v))
    )
  | Abstraction (args, e) ->
    FuncVal (fun in_args ->
        let env' = List.map2 (fun a b -> (a, b)) args in_args @ env
        in value_of_expression env' e
      )
  | Application (e1, es) -> (
      match value_of_expression env e1 with
      | FuncVal f -> f (List.map (fun e -> value_of_expression env e) es)
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
  | Application (e1, es) ->
    "(" ^
    string_of_expression e1 ^ " " ^
    String.concat " " (List.map string_of_expression es) ^
    ")"
  | IfExpression (e1, e2, e3) -> "(if " ^ String.concat " " (List.map string_of_expression [e1; e2; e3]) ^ ")"
  | LetExpression (bindings, body) ->
    let string_of_binding (b, v) = "(" ^ b ^ " " ^ string_of_expression v ^ ")"
    in
    "(let [" ^
    String.concat " " (List.map string_of_binding bindings) ^
    "] " ^
    string_of_expression body ^
    ")"

let func_of_binary_op op =
  FuncVal (function
      | [NumVal a; NumVal b] -> NumVal (op a b)
      | _ -> raise (RuntimeException "Expected exactly two ints."))

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

let rec flatten = function
  | Application (e, es) as orig ->
    [orig]
    @ flatten e
    @ (List.map flatten es |> List.concat)
  | Abstraction (_, e) as orig ->
    [orig]
    @ flatten e
  | IfExpression (e1, e2, e3) as orig ->
    [orig] @ flatten e1 @ flatten e2 @ flatten e3
  | LetExpression (bindings, body) as orig ->
    [orig]
    @ (List.map (fun (_, e) -> flatten e) bindings |> List.concat)
    @ flatten body
  | Number v -> [Number v]
  | String v -> [String v]
  | Boolean v -> [Boolean v]
  | Variable v -> [Variable v]
  | Nil -> [Nil]

let num_exprs expr = flatten expr |> List.length

exception TraversalError
let traverse expr pos =
  match List.nth_opt (flatten expr) pos with
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
  let value = traverse expr pos
  and body = replace { expr = expr;
                       pos = pos;
                       desired = Variable name;
                     }
  in LetExpression ([(name, value)], body)
