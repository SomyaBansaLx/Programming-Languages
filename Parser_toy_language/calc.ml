open Abstract_types
open Parser
open Printf

let string_of_constant c = match c with

| Int i -> "Int " ^ string_of_int i
| String s -> "STRING \"" ^ String.escaped s ^ "\""
| Mylist m -> "list " ^ m

let rec string_of_statement e = match e with 

| Bools z-> z ^ " "

| Cut -> "STOP" 
| Variable v 
-> "Variable \"" ^ v ^ "\""
| Constant c -> "(" ^ (string_of_constant c) ^ ")"
| TermExp (functt, arguments) ->

let func = String.escaped functt in " function (\"" ^ func ^ "\", [" ^

(String.concat ", " (List.map string_of_statement arguments)) ^ "])"

let string_of_exp_list g ="["^(String.concat" AND "(List.map string_of_statement g))^"]"

let string_of_function d = match d with
| Fact me  -> "Fact (" ^ (string_of_statement me) ^ ")"
| Clause (e1, g) ->
  "\n" ^ "Clause (" ^ (string_of_statement e1) ^ "\n" ^ "          IF \n" ^
  (string_of_exp_list g) ^ ")"

let string_of_statements_list d = 
  "{" ^ (String.concat "; " (List.map string_of_function d)) ^ "}" 


let _ =
    let lexbuf = Lexing.from_string "bff(Raj,Me)." in
    
    let result = Parser.program Lexer.token lexbuf in
    print_string (string_of_statements_list result);


     
    (* TEST1
    
    "mem(X,[]) :- mem(Y,up([99|87|R]))."

    {
      Clause ( function ("mem", [Variable "X", (list [])])
                IF 
      [ function ("mem", [Variable "Y",  function ("up", [(list [99|87|R])])])])}
      *)


      (* TEST 2
      bff(Raj,Me) :- male(Me),age(Me,19),!.
      {
Clause ( function ("bff", [Variable "Raj", Variable "Me"])
          IF 
[ function ("male", [Variable "Me"]) AND 
 function ("age", [Variable "Me", (Int 19)]) AND STOP])} *)