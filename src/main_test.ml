open Main;;

let assert_throws fn =
    assert (
        try (
            fn () |> ignore;
            false
        ) with RuntimeException _ -> true
    )

let () =
    assert (NumVal 42 = value_of_expression [] (Number 42));

    assert (NumVal 42 = value_of_expression [("u", NumVal 42)] (Variable "u"));
    assert_throws (fun () -> value_of_expression [] (Variable "u"));

    (* f(x) = x *)
    assert (
        NumVal 42 =
        value_of_expression
            []
            (Application (
                (Abstraction ("arg", Variable "arg"))
              , Number 42)
            ));

    assert_throws (fun () -> value_of_expression [] (
        Application ((String "Not a fn"), Nil))
    );

    assert (NumVal 10 = value_of_expression [] (
        IfExpression ((Boolean true), (Number 10), Number 20)));
    assert (NumVal 20 = value_of_expression [] (
        IfExpression ((Boolean false), (Number 10), Number 20)));
    assert_throws (fun () -> value_of_expression [] (
        IfExpression ((String "not a bool"), Nil, Nil)));

    print_endline "All tests passed."
