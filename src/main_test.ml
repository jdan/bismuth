open Main;;

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
                    (Abstraction ("arg", Variable "arg"))
                , Number 42)
                ));

    assert_throws (fun () -> eval (
        Application ((String "Not a fn"), Nil))
    );

    assert (NumVal 10 = eval (
        IfExpression ((Boolean true), (Number 10), Number 20)));
    assert (NumVal 20 = eval (
        IfExpression ((Boolean false), (Number 10), Number 20)));
    assert_throws (fun () -> eval (
        IfExpression ((String "not a bool"), Nil, Nil)));

    assert (NumVal 42 = eval (
        Application (Application (
            (Variable "+"), Number 10), Number 32)));
    assert_throws (fun () -> eval (
        Application (Application (
            (Variable "+"), Number 10), String "not a number")));

    assert (NumVal 3 = eval (
        Application (Application (
            (Variable "/"), Number 32), Number 10)));
    assert (NumVal 14 = eval (
        Application (Application (
            (Variable "-"), Number 16), Number 2)));

    assert (Number 10 = swap_variable "x" "y" (Number 10));
    assert (Variable "y" = swap_variable "x" "y" (Variable "x"));
    assert (Variable "z" = swap_variable "x" "y" (Variable "z"));

    let orig =
        Abstraction ("x",
            Application
                ( Application (Variable "+", Variable "x")
                , Variable "z"
                )
            )
    in (
        assert (swap_variable "z" "q" orig =
            Abstraction ("x",
                Application
                    ( Application (Variable "+", Variable "x")
                    , Variable "q"
                    )
                ));
        assert (swap_variable "x" "q" orig =
            Abstraction ("q",
                Application
                    ( Application (Variable "+", Variable "q")
                    , Variable "z"
                    )
                ));
    );

    print_endline "All tests passed."
