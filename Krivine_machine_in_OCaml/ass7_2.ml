exception Wrong;;


(*   TYPE DEFINITIONS   *)


type var = string

type expr = 

|INT of int 
|BOOLEAN of bool 

| Var of var
| Abs of var * expr
| App of expr * expr

| Ifthenelse of (expr*expr*expr)
| Fst of expr*expr
| Scnd of expr*expr

| Add of expr*expr
| OR of expr*expr;;


(*   FUNCTION TO PRINT ANSWER  *)


let rec print exp = match exp with 

| INT a -> Printf.printf " Int %d " a
| BOOLEAN b -> Printf.printf " Bool %b " b

| Var x -> Printf.printf " Variable %s " x
| Abs (x,e) -> let () = Printf.printf "Abs %s" x in print e
| App(e1,e2) -> let () = print e1 in print e2 

| Add(e1,e2) -> let () = print e1 in let () = Printf.printf" plus " in print e2 
| OR(e1,e2) -> let () = print e1 in let () = Printf.printf" plus " in print e2 
| _ -> Printf.printf "EMPTY"


type closure = CLOSURE of expr*env
and env = (expr*closure) list;;

type stack = closure list


(*   HELP FUNCTIONS   *)


let rec lookup exp env = match (exp, env) with

| (exp, []) -> raise Wrong

| (exp, (e1,clos)::rem) -> 
      if e1=exp then 
					(match clos with
					| CLOSURE (Abs (x,x1), env) -> CLOSURE (Abs (x, x1), (e1,clos)::env)
					| _ -> clos)
      else (lookup exp rem) 
;;


let addi num_1 num_2 = match (num_1,num_2) with

| (CLOSURE(INT i1, table_1), CLOSURE (INT i2, table_2)) -> CLOSURE (INT (i1+i2), [])
| _-> raise Wrong;;



let ori bool_1 bool_2 = match (bool_1,bool_2) with
| (CLOSURE(BOOLEAN i1, env1), CLOSURE (BOOLEAN i2, env2)) -> CLOSURE (BOOLEAN (i1||i2), [])
| _-> raise Wrong;;



let abstraction clos stack = match (clos, stack) with

| (_,[]) -> raise Wrong
| (CLOSURE (Abs(x ,exp), env), clos::rem) -> (CLOSURE (exp, (Var x, clos)::env), rem)
| _ -> raise Wrong;;



(* MAIN FUNCTION *)


let rec kriv clos (s:stack) = match clos with

| CLOSURE(INT i, env) -> CLOSURE(INT i, env)
| CLOSURE(BOOLEAN b, env) -> CLOSURE(BOOLEAN b, env)

| CLOSURE(Var v, env) -> kriv (lookup (Var v) env) s
| CLOSURE(Abs(x, e), env) -> 
					let (cl', s') = (abstraction clos s) in
						kriv cl' s'
| CLOSURE(App(e1, e2), env) -> kriv (CLOSURE (e1, env)) (CLOSURE (e2, env)::s)

| CLOSURE(Ifthenelse(e0, e1, e2), env) ->
				let a0 = (kriv (CLOSURE (e0, env)) []) in
					(match a0 with
					| CLOSURE(BOOLEAN b, env) -> (if b then (kriv (CLOSURE(e1, env)) s) else (kriv (CLOSURE(e2, env)) s))
					| _ -> raise Wrong)

| CLOSURE(Fst(e1,e2),env) -> kriv (CLOSURE(e1, env)) s
| CLOSURE(Scnd(e1,e2),env) -> kriv (CLOSURE(e2, env)) s

| CLOSURE(OR(e1, e2), env) -> (kriv (ori (kriv (CLOSURE (e1, env)) []) (kriv (CLOSURE (e2, env)) [])) s)
| CLOSURE(Add(e1, e2), env) -> (kriv (addi (kriv (CLOSURE (e1, env)) []) (kriv (CLOSURE (e2, env)) [])) s)
      

(*    TEST CASES     *)
  let my_test_1 = App(Abs("x",INT 3),
        Add(INT 3,Var "y"));; 
      
  let my_test_2 = App(Abs("x", 
      Add(Var "x", (Add(INT 10, Var "x")))),
         INT 3);; 
        
  let my_test_3 = App(Abs("x", Add(Var "x", (Add(Ifthenelse(BOOLEAN true, INT 10, INT 15), Var "x")))), INT 3);; 

  let my_test_4 = App(Abs("x", 
             Ifthenelse(BOOLEAN true, 
                 Add(Var "x", Ifthenelse(BOOLEAN false, INT 5, INT 10)),
                 OR(BOOLEAN false, BOOLEAN true)
             )
           ), INT 3);;



  let help_1 = Abs("f", Abs("x", App(Var "f", App(Var "f", Var "x"))));;
  let help_2 = Abs("x", Add(Var "x", INT 1));;         
  let my_test_5 = App(App(help_1, help_2), INT 5);;


  let me = kriv (CLOSURE(my_test_5,[])) [] ;;

  let print_closure (c:closure) = match c with 
| CLOSURE(e,_) -> print e 


  let () = print_closure me;;