open Opal
open Lang

(* Atoms *)
let nil_lit = token "nil" => (fun _ -> Nil)

let true_lit = token "#t" => (fun _ -> Boolean true)
let false_lit = token "#f" => (fun _ -> Boolean false)
let boolean_lit = true_lit <|> false_lit

let number_lit = spaces >> many1 digit => implode % int_of_string % (fun i -> Number i)
let string_lit =
  let quot = exactly '"'
  in spaces >> between quot quot (many1 any) => implode % (fun s -> Lang.String s)

let binding =
  let symbol = one_of ['\''; '"'; '-'; '_'; '+'; '*'; '?'; '=']
  in spaces >> many1 (digit <|> letter <|> symbol) => implode

let variable_lit =
  binding => fun v -> Variable v

(* Now the fun stuff *)
let lparen = token "("
let rparen = token ")"
let rec if_expression input =
  ( lparen >>
    token "if" >>
    expr >>= fun condition ->
    expr >>= fun consequent ->
    expr >>= fun alternate ->
    rparen >>
    return (IfExpression (condition, consequent, alternate))
  ) input
and _let_expression input =
  let single = ( lparen >>
                 binding >>= fun binding ->
                 expr >>= fun value ->
                 rparen >>
                 return (binding, value)
               ) in
  ( lparen >>
    token "let" >>
    token "[" >>
    many1 single >>= fun bindings ->
    token "]" >>
    expr >>= fun body ->
    rparen >>
    return (LetExpression (bindings, body))
  ) input
and abstraction input =
  ( lparen >>
    token "fn" >>
    lparen >>
    many1 binding >>= fun args ->
    rparen >>
    expr >>= fun body ->
    rparen >>
    return (Abstraction (args, body))
  ) input
and named_abstraction input =
  ( lparen >>
    token "fun" >>
    lparen >>
    binding >>= fun name ->
    many1 binding >>= fun args ->
    rparen >>
    many1 expr >>= fun seq ->
    rparen >>
    return (NamedAbstraction (name, args, seq))
  ) input
and application input =
  ( lparen >>
    expr >>= fun fn ->
    many1 expr >>= fun args ->
    rparen >>
    return (Application (fn, args))
  ) input
and expr input =
  ( nil_lit <|> number_lit <|> string_lit <|> boolean_lit <|> variable_lit <|>
    if_expression <|> _let_expression <|> abstraction <|> named_abstraction <|> application
  ) input
and program input =
  ( many expr >>= fun exprs ->
    return exprs
  ) input

exception ParseException
let parse input = match Opal.parse program (LazyStream.of_string input) with
  | Some res -> res
  | None -> raise ParseException
