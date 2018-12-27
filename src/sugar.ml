open Lisp;;

let _apply f args =
  (* Apply a list of expressions to a curried function *)
  let rec inner acc = function
    | [] -> acc
    | arg::args -> inner (Application (acc, arg)) args
  in inner f args

let rec _let bindings body =
  (* let x = e in f <=> ((fn (x) f) e) *)
  let single binding exp body =
    Application (( Abstraction (binding, body)
                 , exp
                 ))
  in match bindings with
  | [] -> body
  | (v, e)::bindings' ->
    single v e (
      _let bindings' body
    )
