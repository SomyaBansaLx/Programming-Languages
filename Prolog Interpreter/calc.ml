open Abstract_types
open Lexer
open Parser
open Printf

exception UNABLE_TO_UNIFY
let string_of_constant c = match c with

| Int i -> "Int " ^ string_of_int i
| String s -> "STRING \"" ^ String.escaped s ^ "\""
;;


let rec string_of_statement e = match e with 

| Bools z-> z ^ " "

| Cut -> "STOP" 
| Variable v 
-> "Variable \"" ^ v ^ "\""
| Constant c -> "(" ^ (string_of_constant c) ^ ")"
| TermExp (functt, arguments) ->
"[" ^



(String.concat ", " (List.map string_of_statement arguments)) ^ "])"

let string_of_exp_list g ="["^(String.concat" AND "(List.map string_of_statement g))^"]"


let string_of_function d = match d with
| Clause (e1, g) ->
  "\n" ^ "Clause (" ^ (string_of_statement e1) ^ "\n" ^ "          IF \n" ^
  (string_of_exp_list g) ^ ")"


let rec readable_string_of_exp e = match e with
| Variable v -> v
| Constant c -> string_of_constant c 
| TermExp (s, l) -> s ^ "(" ^ ( String.concat ", " ( List.map readable_string_of_exp l)) ^ ")"
| _ -> ""
  

let string_of_statements_list d = 
  "{" ^ (String.concat "; " (List.map string_of_function d)) ^ "}" 

  let rec find_vars q = match q with
  | [] -> []
  | (x :: xs) -> (match x with
        | Variable v -> x :: (find_vars xs)
        | TermExp (s, el) -> (find_vars el) @ (find_vars xs)
        | _ -> (find_vars xs)
)


let rec find_vars_string q = match q with
| [] -> []
| (x :: xs) -> (match x with
      | Variable v -> v :: (find_vars_string xs)
      | TermExp (s, el) -> (find_vars_string xs)@(find_vars_string el) 
      | _ -> (find_vars_string xs)
)


let unique l =
  let rec remove_duplicates acc = function
    | [] -> List.rev acc 
    | hd :: tl ->
      if List.mem hd acc then remove_duplicates acc tl
      else remove_duplicates (hd :: acc) tl
  in
  remove_duplicates [] l



let rec hehe_1 sub gg =
  match gg with
  | Variable v -> (
      try
        let i = List.assoc gg sub in
        i
      with Not_found ->
        Variable v
    )
  | TermExp (s, el) ->
      TermExp (s, List.map (fun g1 -> hehe_1 sub g1) el)
  | _ -> gg



let hehe_2 sub gl =
  List.map (fun g1 -> hehe_1 sub g1) gl

  let (fresh2 : statement->string) =
  let counter = ref 0 in
  fun prefix ->
    match prefix with
    | Variable v->
      let current_counter = !counter in
      counter := current_counter + 1;
      v ^ (string_of_int current_counter)
    | _ -> "none"



let rec rename_vars_in_clause d = match d with

  Clause (h, b) ->
  let head_vars = find_vars [h] in
  let body_vars = find_vars b in
  let vars = unique (head_vars @ body_vars) in
  let sub = List.map (fun x -> (x, Variable (fresh2 x))) vars in 
  Clause (hehe_1 sub h, hehe_2 sub b)


let rec rename_vars_in_query (q:query):query = match q with

  Query(b) ->let body_vars = find_vars b in
  let vars = unique (body_vars) in
  let sub = List.map (fun x -> (x, Variable (fresh2 x))) vars in 
  Query(hehe_2 sub b)



let rec pair_krdo arg1 arg2 c = match arg1 with
| [] -> 
  if (List.length arg2 = 0)
  then c
  else raise (Failure "WRONG")
| (s :: ss) ->  
  match arg2 with
  | (t :: ts) -> pair_krdo ss ts ((s, t) :: c)
  | _ -> raise (Failure "WRONG")
              

let rec replace c ss = match c with
  | [] -> []
  | ((s, t) :: remm) ->
      (hehe_1 ss s,hehe_1 ss t) :: (replace remm ss)


let rec exist n t = match t with
    | Variable m -> n = m
    | TermExp (st, el) ->
      List.fold_left (fun acc v -> acc || (exist n v)) false el
      | _ -> false

let string_me e query_vars = let res =
  List.fold_left (fun r2 env -> let sol = "*\n" ^ 
  (List.fold_left 
  (fun r d -> match d with
      | Variable v ->(try let f = List.assoc (Variable v) env in
                match f with
                | Variable v2 -> (v ^ "\n") ^ r
                | _ -> (v ^ " = " ^ (string_of_statement f) ^ "\n") ^ r
                      with Not_found -> (v ^ "\n") ^ r)
      | _ -> r) "" query_vars) ^ "*\n"  in
        sol ^ r2) 
  (if List.length e > 0 then "TRUE\n" else "FALSE\n") e   
in res

    
let rec simplify_term (t:statement): statement = match t with
  | TermExp("plus", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Constant(Int n1), Constant(Int n2)) -> Constant(Int(n1 + n2))
        | _ -> t
    )
  | TermExp("multiply", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Constant(Int n1), Constant(Int n2)) -> Constant(Int(n1 * n2))
        | _ -> t
    )
  | TermExp("minus", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Constant(Int n1), Constant(Int n2)) -> Constant(Int(n1 - n2))
        | _ -> t
    )
  | _ -> t

let rec unify_kro constraints = match constraints with

  | [] -> Some []
  | (s, t) :: other ->
    let s' = (simplify_term s) in let t' = (simplify_term t) in 

      if s' = t' then unify_kro other
      else match s', t' with

    | Variable n, _ ->

        if exist n t then None
        else
          let usee = [(s,t)] in let other' = replace other usee in
          let usee_2 = unify_kro other' in
          (match usee_2 with
          | None -> None
          | Some l -> Some ((s, hehe_1 l t) :: l)
          )

    | TermExp (name, arg), Variable k -> unify_kro ((t, s) :: other)

    | TermExp (name_1, arg_1), TermExp (name_2, arg_2) ->

        if name_1 = name_2 && List.length arg_1 = List.length arg_2 then
          unify_kro (pair_krdo arg_1 arg_2 other)
        else None

    | _, Variable k -> unify_kro ((t, s) :: other)

    | _ -> None


let rec eval_query (my_query, programm, environment) varss = match my_query with

| [] -> [environment]

| (first_query :: rem_query) -> ( 
let vars_list_string = unique (varss@(find_vars_string my_query))  in

(* let meee = List.map (print_string) vars_list_string in  *)
let env_2 =
  List.filter (fun (variable, _) ->
      match variable with
      | Variable var_use -> List.mem var_use vars_list_string
      | _ -> false
    ) environment in 

match first_query with

| TermExp("true", []) -> ( eval_query ( rem_query, programm, env_2 ) varss )
| TermExp(_,_) -> ( List.fold_right (

fun rule r -> (
match (rename_vars_in_clause rule) with 

| Clause (h, b) -> (
match unify_kro [(first_query, h)] with 
| Some s -> (
match unify_kro (s@env_2) with | Some env2 -> (

if (List.length b = 1) then 
  match b with
  | ((TermExp ("true", _)) :: ys) -> ((eval_query (hehe_2 s rem_query, programm,env2) varss) @ r)
  | (Bools "fail"::rem) -> r
  | _ -> ((eval_query ((hehe_2 s b) @ (hehe_2 s rem_query),programm, env2) varss) @ r)

else

(eval_query ((hehe_2 s b) @ (hehe_2 s rem_query),programm,env2) varss) @ r)

| _ -> r )
                   
| _ -> r )
                
)) programm [] )
       
| _ -> eval_query (rem_query, programm, env_2) varss

)


let eval ((dec:query), db):unit = match dec with

| Query(b) -> (

let varss = unique (find_vars b) in
let vars_ass_string = unique(find_vars_string b)  in
let resultt = eval_query (b, db, []) vars_ass_string in
let result_string = string_me resultt varss in
print_string result_string;

)

