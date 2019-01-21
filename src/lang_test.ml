open Lang

let assert_throws fn =
  assert (
    try (
      fn () |> ignore;
      false
    ) with RuntimeException _ -> true
  )

let rec cons_of_int_list = function
  | [] -> NilVal
  | x::xs -> ConsVal (NumVal x, cons_of_int_list xs)

let run () =
  assert (NumVal 42 = eval [Number 42]);

  assert (NumVal 42 = value_of_expression [("u", NumVal 42)] (Variable "u"));
  assert_throws (fun () -> eval [Variable "u"]);

  (* if *)
  assert (NumVal 10 = eval [
      IfExpression ((Boolean true), (Number 10), Number 20)
    ]);
  assert (NumVal 20 = eval [
      IfExpression ((Boolean false), (Number 10), Number 20)
    ]);
  assert_throws (fun () -> eval [
      IfExpression ((String "not a bool"), Nil, Nil)
    ]);

  (* application *)
  assert (NumVal 42 =
          eval
            [Application (
                (Abstraction (["arg"], Variable "arg"))
              , [Number 42])
            ]);

  assert_throws (fun () -> eval [
      Application ((String "Not a fn"), [Nil])
    ]);

  assert (NumVal 42 = eval [
      Application ( Variable "+"
                  , [Number 10; Number 32])
    ]);
  assert_throws (fun () -> eval [
      Application ( Variable "+"
                  , [Number 10; String "not a number"])
    ]);

  assert (NumVal 3 = eval [
      Application ( Variable "/"
                  , [Number 32; Number 10])
    ]);
  assert (NumVal 14 = eval [
      Application ( Variable "-"
                  , [Number 16; Number 2])
    ]);
  assert (NumVal 900 = eval [
      Application ( Variable "-"
                  , [Number 1000; Number 100])
    ]);

  let sub3 = FuncVal (function
      | [NumVal a'; NumVal b'; NumVal c'] -> NumVal (a' - b' - c')
      | _ -> raise (RuntimeException "Sorry"))
  in assert (NumVal 890 = value_of_expression [("-", sub3)] (
      Application ( Variable "-",
                    [Number 1000; Number 100; Number 10])));

  (* various wholesome tests *)
  assert (NumVal 7 = (Parser.parse "(+ 3 4)" |> eval));
  assert (NumVal 42 = (Parser.parse "(* 6 7)" |> eval));
  assert (NumVal 42 = (Parser.parse "((fn (x) (+ x 5)) 37)" |> eval));
  assert (NumVal 20 = (Parser.parse "(let [(x 5) (y 15)] (+ x y))" |> eval));

  (* fun *)
  assert (NumVal 120 = (Parser.parse "
    (fun (factorial n)
      (if (= n 0)
          1
          (* n
             (factorial (- n 1)))))
    (factorial 5)" |> eval)) ;

  (* lists *)
  assert (
    cons_of_int_list [1; 2; 3; 4] =
    (Parser.parse "
      (fun (upto n)
        (fun (inner a b)
          (if (= a b)
            nil
            (cons a (inner (+ a 1) b))))
        (inner 1 n))
      (upto 5)
    " |> eval));

  assert (
    BoolVal true =
    (Parser.parse "(= (cons 5 4) (cons (+ 3 2) 4))" |> eval));
  assert (
    BoolVal false =
    (Parser.parse "(= (cons 5 4) (cons (+ 3 2) 5))" |> eval));

  assert (cons_of_int_list [2; 3; 5; 7; 11; 13] =
          (Parser.parse "(list 2 3 5 7 11 13)" |> eval));
  assert (cons_of_int_list [3; 5; 7; 11; 13] =
          (Parser.parse "(cdr (list 2 3 5 7 11 13))" |> eval));
  assert (NumVal 3 =
          (Parser.parse "(cadr (list 2 3 5 7 11 13))" |> eval));
  assert (NumVal 5 =
          (Parser.parse "(caddr (list 2 3 5 7 11 13))" |> eval));

  assert (NumVal 45 =
          (Parser.parse "(caadr (list #t (list 45 #f 3)))" |> eval));

  (* printing *)
  assert ("(let [(x 5) (y 15)] (+ x y))" =
          (Parser.parse "(let [(x 5)     (y 15)] (+ x y))" |> string_of_program));
  assert ("(if #t 3 5)" =
          (Parser.parse "(if #t \n 3  \t 5)" |> string_of_program));
