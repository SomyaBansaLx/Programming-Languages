{

    open Parser
    exception EndInput

}

let upper = ['A' - 'Z']  
let underline = '_'
let lower = ['a' - 'z'] 
let digit = ['0' - '9']
let alphanumeric = upper | underline | lower | digit 
let atom = lower ( alphanumeric * )
let digits = ['1'-'9'] 
let sign = ['+' '-']
let Myy_integers = (digit | (sign ? (digits digit *)))
let variable = (upper | underline) alphanumeric *
let element = (variable|Myy_integers) '|'

rule token = parse

| [' ' '\t' '\n']  { token lexbuf }
| eof { EOF }
| '!' { CUT }
| '[' { LEFTSQUARE }
| ']' { RIGHTSQUARE }
| "true"|"fail"|"false" as a { BOOLS a}
| atom as a { ATOM a }
| ',' { COMMA }
| '|' { DANDA }
| Myy_integers as n  { INT (int_of_string n) }
| variable as v  { VARIABLE v }
| ":-" { RULE }
| '.' { FULLSTOP }
|'('  { LEFTCURVE } 
| ')' { RIGHTCURVE }
| '?' {QUESTION_MARK}
| '+' {PLUS}
| '*' {MULTIPLY}
| '-' {MINUS}