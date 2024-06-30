type constant =
| Int of int 
| String of string 

type statement =
| Cut
| Bools of string
| Variable of string 
| Constant of constant  
| TermExp of string * (statement list)

type functions =
| Clause of statement * (statement list) 

type prog = functions list 

type query = 
| Query of statement list
