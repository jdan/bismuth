type token =
  | LPAREN
  | RPAREN
  | INT of int
  | STRING of string
  | BOOL of bool
  | IDENTIFIER of string

let string_of_token = function
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | INT(x) -> "INT(" ^ string_of_int x ^ ")"
  | STRING(str) -> "STRING(\"" ^ str ^ "\")"
  | BOOL(b) -> "BOOL(" ^ string_of_bool b ^ ")"
  | IDENTIFIER(i) -> "IDENTIFIER(" ^ i ^ ")"

let string_of_tokens tokens =
  "[" ^ String.concat "; " (List.map string_of_token tokens) ^ "]"

(* https://caml.inria.fr/mantis/view.php?id=5367 *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let implode l =
  let res = Bytes.create (List.length l) in
  let rec imp i = function
    | [] -> res
    | c :: l ->  Bytes.set res i c; imp (i + 1) l in
  Bytes.to_string (imp 0 l)

exception TokenizeException of string

let extract_string =
  (* basically takeWhile p -> p <> '"' *)
  let rec inner acc = function
    | [] -> (acc, [])
    | '"'::' '::rest -> (acc, rest)
    | '"'::_::rest as whole -> raise (TokenizeException (
        "Unable to extract string from \"" ^
        (implode acc) ^
        (implode whole)
      ))
    | c::rest -> inner (c::acc) rest
  in inner []

let matches_int = function
  | [] -> false
  | c::_ -> (match String.index_opt "0123456789" c with
      | None -> false
      | _ -> true
    )

let matches_ident = function
  | [] -> false
  | c::_ -> (
      let valid_chars =
        "abcdefghijklmnopqrstuvwxyz" ^
        "0123456789" ^
        "_+*'?-"
      in match String.index_opt valid_chars c with
      | None -> false
      | _ -> true
    )

let extraction_of_matcher fn =
  let rec inner = function
    | [] -> ([], [])
    | ' '::rest -> ([], rest)
    | ')'::_ as whole -> ([], whole)
    | c::rest as whole ->
      if fn whole
      then
        let (acc, rest') = inner rest
        in (c::acc, rest')
      else raise (TokenizeException (
          "Unable to extract int at " ^
          (c |> String.make 1)
        ))
  in inner

let extract_int = extraction_of_matcher matches_int
let extract_ident = extraction_of_matcher matches_ident

let tokens_of_string str =
  let rec inner = function
    | [] -> []
    | ' '::rest -> inner rest
    | '('::rest -> LPAREN :: inner rest
    | ')'::rest -> RPAREN :: inner rest
    | '#'::'t'::rest -> BOOL true ::  inner rest
    | '#'::'f'::rest -> BOOL false ::  inner rest
    | '"'::rest ->
      let (str, rest') = extract_string rest
      in STRING (implode str) :: inner rest'
    | other ->
      if matches_int other then
        let (i, rest) = extract_int other
        in INT (implode i |> int_of_string) :: inner rest
      else if matches_ident other then
        let (i, rest) = extract_ident other
        in IDENTIFIER (implode i) :: inner rest
      else
        raise (TokenizeException (List.hd other |> String.make 1))

  in inner (explode str)
