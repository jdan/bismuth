open Lisp

let assert_throws fn =
  assert (
    try (
      fn () |> ignore;
      false
    ) with RuntimeException _ -> true
  )

let () =
  assert (NumVal 42 = eval (Number 42));

  assert (NumVal 42 = value_of_expression [("u", NumVal 42)] (Variable "u"));
  assert_throws (fun () -> eval (Variable "u"));

  (* f(x) = x *)
  assert (NumVal 42 =
          eval
            (Application (
                (Abstraction (["arg"], Variable "arg"))
              , [Number 42])
            ));

  assert_throws (fun () -> eval (
      Application ((String "Not a fn"), [Nil]))
    );

  assert (NumVal 10 = eval (
      IfExpression ((Boolean true), (Number 10), Number 20)));
  assert (NumVal 20 = eval (
      IfExpression ((Boolean false), (Number 10), Number 20)));
  assert_throws (fun () -> eval (
      IfExpression ((String "not a bool"), Nil, Nil)));

  assert (NumVal 42 = eval (
      Application ( Variable "+"
                  , [Number 10; Number 32])));
  assert_throws (fun () -> eval (
      Application ( Variable "+"
                  , [Number 10; String "not a number"])));

  assert (NumVal 3 = eval (
      Application ( Variable "/"
                  , [Number 32; Number 10])));
  assert (NumVal 14 = eval (
      Application ( Variable "-"
                  , [Number 16; Number 2])));
  assert (NumVal 900 = eval (
      Application ( Variable "-"
                  , [Number 1000; Number 100])));

  let sub3 = FuncVal (function
      | [NumVal a'; NumVal b'; NumVal c'] -> NumVal (a' - b' - c')
      | _ -> raise (RuntimeException "Sorry"))
  in assert (NumVal 890 = value_of_expression [("-", sub3)] (
      Application ( Variable "-",
                    [Number 1000; Number 100; Number 10])));

  assert (NumVal 42 = eval (
      _let [("x", Number 10); ("y", Number 32)] (
        Application ( Variable "+"
                    , [Variable "x"; Variable "y"]
                    )
      )));

  assert (Number 10 = swap_variable "x" "y" (Number 10));
  assert (Variable "y" = swap_variable "x" "y" (Variable "x"));
  assert (Variable "z" = swap_variable "x" "y" (Variable "z"));

  let orig =
    Abstraction
      ( ["x"]
      , Application (Variable "+", [Variable "x"; Variable "z"])
      )
  in (
    assert (swap_variable "z" "q" orig =
            Abstraction
              ( ["x"]
              , Application ( Variable "+"
                            , [Variable "x"; Variable "q"]
                            ))
           );
    assert (swap_variable "x" "q" orig = orig);
  );

  assert (NumVal 7 = (Parser.parse "(+ 3 4)" |> eval));
  assert (NumVal 42 = (Parser.parse "(* 6 7)" |> eval));
  assert (NumVal 42 = (Parser.parse "((fn (x) (+ x 5)) 37)" |> eval));
  assert (NumVal 20 = (Parser.parse "(let [(x 5) (y 15)] (+ x y))" |> eval));

  assert ("(let [(x 5) (y 15)] (+ x y))" = (Parser.parse "(let [(x 5)     (y 15)] (+ x y))" |> string_of_expression));
  assert ("(if #t 3 5)" = (Parser.parse "(if #t \n 3  \t 5)" |> string_of_expression));

  assert (NumVal 42 =
          eval_program
            [ Definition (ValueDefinition ("a", Number 10))
            ; Definition (ValueDefinition ("b", Number 32))
            ; Expression (Application (Variable "+", [Variable "a"; Variable "b"]))
            ]
         );

  (* Recursive fibonacci *)
  assert (NumVal 120 =
          eval_program
            [ Definition
                (FunctionDefinition
                   ("fib", ["n"], [],
                    IfExpression
                      ( Application (Variable "=", [Number 0; Variable "n"])
                      , Number 1
                      , Application
                          ( Variable "*"
                          , [ Variable "n"
                            ; Application
                                ( Variable "fib"
                                , [Application (Variable "-", [Variable "n"; Number 1])]
                                )
                            ]
                          )
                      )))
            ; Expression (Application (Variable "fib", [Number 5]))
            ]
         );

  (* Tail-call optimized recursive fibonacci *)
  let fib = [ Definition
                (FunctionDefinition
                   ("fib", ["n"],
                    [ FunctionDefinition ("inner", ["n"; "acc"], [], (
                          IfExpression
                            ( Application (Variable "=", [Number 0; Variable "n"])
                            , Variable "acc"
                            , Application
                                ( Variable "inner"
                                , [ Application (Variable "-", [Variable "n"; Number 1])
                                  ; Application (Variable "*", [Variable "acc"; Variable "n"])
                                  ]
                                )
                            )))
                    ],
                    Application (Variable "inner", [Variable "n"; Number 1])
                   ))
            ; Expression (Application (Variable "fib", [Number 5]))
            ]
  in (
    assert (NumVal 120 = eval_program fib);
    assert (
      let (_, env) = value_env_of_program stdlib fib
      in let env_increase = List.length env - List.length stdlib
      in 1 = env_increase   (* assert inner is not in `env` *)
    )
  );

  (* TODO: test that
     - arguments can be overwritten by inner definitions
     - (define (f f) f)
  *)

  assert (7 = (Parser.parse "(let [(x 5) (y 15)] (+ x y))" |> num_exprs));

  assert (
    (Parser.parse "(let [(x 5) (y 15)] (+ x y))"
     |> flatten
     |> List.mapi (fun i e ->
         "("
         ^ string_of_int i
         ^ ") "
         ^ string_of_expression e
       )
    )
    = [ "(0) (let [(x 5) (y 15)] (+ x y))"
      ; "(1) 5"
      ; "(2) 15"
      ; "(3) (+ x y)"
      ; "(4) +"
      ; "(5) x"
      ; "(6) y"
      ]);

  assert (Variable "+" = (traverse (Parser.parse "(let [(x 5) (y 15)] (+ x y))") 4));

  assert (
    Parser.parse "(let [(x 5) (y 15)] (NEW x y))" =
    replace
      { expr = (Parser.parse "(let [(x 5) (y 15)] (+ x y))");
        desired = (Variable "NEW");
        pos = 4;
      }
  );

  assert (
    Parser.parse "(let [(x 5) (y 15)] (+ x NEW))" =
    replace
      { expr = (Parser.parse "(let [(x 5) (y 15)] (+ x y))");
        desired = (Variable "NEW");
        pos = 6;
      }
  );

  assert (
    Parser.parse "(let [(x 5) (y NEW)] (+ x y))" =
    replace
      { expr = (Parser.parse "(let [(x 5) (y 15)] (+ x y))");
        desired = (Variable "NEW");
        pos = 2;
      }
  );

  assert (
    Parser.parse "(let [(x 10)] (+ x 15))" =
    abstract
      { expr = (Parser.parse "(+ 10 15)");
        name = "x";
        pos = 2;
      }
  );

  print_endline "All tests passed."
