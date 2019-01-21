open Lang
open Transform
open Parser

let run () =
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

  assert (7 =
          (Parser.parse "(let [(x 5) (y 15)] (+ x y))" |> num_exprs));

  assert (Variable "+" = (traverse (Parser.parse "(let [(x 5) (y 15)] (+ x y))") 4));

  assert (
    Parser.parse "(let [(x 5) (y 15)] (NEW x y))" =
    [ replace
        { expr = Parser.parse "(let [(x 5) (y 15)] (+ x y))" |> List.hd;
          desired = (Variable "NEW");
          pos = 4;
        }
    ]
  );

  assert (
    Parser.parse "(let [(x 5) (y 15)] (+ x NEW))" =
    [ replace
        { expr = Parser.parse "(let [(x 5) (y 15)] (+ x y))" |> List.hd;
          desired = (Variable "NEW");
          pos = 6;
        }
    ]
  );

  assert (
    Parser.parse "(let [(x 5) (y NEW)] (+ x y))" =
    [ replace
        { expr = Parser.parse "(let [(x 5) (y 15)] (+ x y))" |> List.hd;
          desired = (Variable "NEW");
          pos = 2;
        }
    ]
  );

  assert (
    Parser.parse "(let [(x 10)] (+ x 15))" =
    [ abstract
        { expr = Parser.parse "(+ 10 15)" |> List.hd;
          name = "x";
          pos = 2;
        }
    ]
  );
