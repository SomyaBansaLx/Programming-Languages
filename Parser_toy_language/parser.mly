%{
    open Abstract_types;;
%}

%token COMMA
%token DANDA 
%token EOF
%token CUT
%token FULLSTOP

%token <int> INT
%token <string> STRING ATOM
%token <string> VARIABLE BOOLS MYLIST

%token RULE 

%token LEFTCURVE
%token RIGHTCURVE
%token LEFTSQUARE
%token RIGHTSQUARE


%start program

%type <Abstract_types.functions> clause
%type <Abstract_types.prog> program
%type <Abstract_types.statement> predicate term 
%type <Abstract_types.statement list> term_list predicate_list 
%type <Abstract_types.constant> constant
%%

clause:
| predicate FULLSTOP  { Fact($1) }
| predicate RULE predicate_list FULLSTOP { Clause ($1, $3) }

program:
| clause {[$1]}
| clause program {$1::$2}

predicate :
| BOOLS {Bools $1}
| CUT { Cut }
| ATOM LEFTCURVE term_list RIGHTCURVE  { TermExp ($1, $3) }

predicate_list:
| predicate  { [$1] }
| predicate COMMA  predicate_list  { $1 :: $3 }

term:
| constant  { Constant $1 }
| VARIABLE {Variable $1}
| predicate  { $1 }

term_list:
| term { [$1] }
| term COMMA term_list { $1 :: $3 }

constant:
| INT  { Int $1 }
| STRING  { String $1 }
| MYLIST {Mylist $1}


