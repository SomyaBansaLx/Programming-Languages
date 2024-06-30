type constant =
| Int of int 
| String of string 
| Mylist of string

type statement =
| Cut
| Bools of string
| Variable of string 
| Constant of constant  
| TermExp of string * statement list 

type functions =
| Fact of statement
| Clause of statement * (statement list) 

type prog = functions list 