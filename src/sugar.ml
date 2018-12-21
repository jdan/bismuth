open Lisp;;

let _apply f args =
    (* Apply a list of expressions to a curried function *)
    let rec inner acc = function
        | [] -> acc
        | arg::args -> inner (Application (acc, arg)) args
    in inner f args

(* let x = e in f <=> ((fn (x) f) e) *)
let _let binding exp body =
    (Application (
        ( Abstraction (binding, body)
        , exp
        )
    ))

let rec _let' bindings body = match bindings with
    | [] -> body
    | (v, e)::bindings' ->
        _let v e (
            _let' bindings' body
        )