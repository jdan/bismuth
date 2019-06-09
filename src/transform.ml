open Lang

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

let rec pretty_string_of_expression exp =
  let rec next_to str = function
    | [] -> [str]
    | hd::tl ->
      let indent = String.make (1 + String.length str) ' '
      in (str ^ " " ^ hd ) ::
         List.map (fun s -> indent ^ s) tl

  and indent spaces = next_to (String.make (spaces - 1) ' ')

  and add_end str = function
    | [] -> [str]
    | hd::[] -> [hd ^ str]
    | hd::tl -> hd :: add_end str tl

  and inner = function
    | Nil -> ["nil"]
    | Number v -> [string_of_int v]
    | String v -> ["\"" ^ v ^ "\""]
    | Boolean true -> ["#t"]
    | Boolean false -> ["#f"]
    | Variable v -> [v]
    | Abstraction (args, e) ->
      let args = "(" ^ (String.concat " " args) ^ ")"
      and rest = add_end ")" (inner e)
      in next_to "(fn" (args :: rest)

    | NamedAbstraction (name, args, es) ->
      let name_and_args = "(" ^ name ^ " " ^ (String.concat " " args) ^ ")"
      and rest = add_end ")" (List.map inner es |> List.flatten)
      in (
        ("(fun " ^ name_and_args) ::
        indent 4 rest
      )

    | Application (e1, es) -> (
        match inner e1 with
        | hd::[] ->
          (* TODO: see if hd next to the top of es is short enough *)
          next_to
            ("(" ^ hd)
            (add_end ")" (List.map inner es |> List.flatten))
        | p1 ->
          let rest = List.map inner es |> List.flatten
          (* TODO: see if rest can fit on one line *)
          in next_to "(" (List.append p1 rest) |> add_end ")"
      )

    | IfExpression (e1, e2, e3) ->
      let clauses = List.map inner [e1; e2; e3] |> List.flatten
      in next_to "(if" clauses |> add_end ")"

    | LetExpression (bindings, body) ->
      (* TODO *)
      let string_of_binding (b, v) = "(" ^ b ^ " " ^ string_of_expression v ^ ")"
      in
      ["(let [" ^
       String.concat " " (List.map string_of_binding bindings) ^
       "] " ^
       string_of_expression body ^
       ")"]
  in String.concat "\n" (inner exp)

and pretty_string_of_program exprs =
  String.concat "\n\n" (List.map pretty_string_of_expression exprs)
