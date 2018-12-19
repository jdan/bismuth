open Main;;

let () =
    assert (NumVal 42 = value_of_expression [] (Number 42));

    (* f(x) = x *)
    assert (
        NumVal 42 =
        value_of_expression
            []
            (Application (
                (Abstraction ("arg", Variable "arg"))
              , Number 42)
            ));
    print_endline "All tests passed."
