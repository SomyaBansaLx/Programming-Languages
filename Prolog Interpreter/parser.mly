%{
    open Abstract_types;;
    exception ParsingError of string
%}




%token COMMA
%token DANDA 
%token EOF
%token CUT
%token FULLSTOP
%token PLUS
%token MINUS
%token MULTIPLY
%token QUESTION_MARK

%token <int> INT
%token <string> ATOM 
%token <string> VARIABLE BOOLS MYLIST

%token RULE 

%token LEFTCURVE
%token RIGHTCURVE
%token LEFTSQUARE
%token RIGHTSQUARE


%start program queryy

%type <Abstract_types.functions> clause
%type <Abstract_types.prog> program
%type <Abstract_types.query> queryy
%type <Abstract_types.statement> predicate term 
%type <Abstract_types.statement list> term_list predicate_list 
%type <Abstract_types.constant> constant

%%

clause:
| predicate FULLSTOP  {  Clause ($1, [TermExp ("true", [])]) }
| predicate RULE predicate_list FULLSTOP { Clause ($1, $3) }
| error { raise (ParsingError "Syntax error") }

program:
| clause {[$1]}
| clause program {$1::$2}

queryy:
|QUESTION_MARK predicate_list FULLSTOP { Query($2)}

predicate :
| BOOLS {Bools $1}
| CUT { Cut }
| ATOM LEFTCURVE term_list RIGHTCURVE  { TermExp ($1, $3) }

predicate_list:
| predicate  { [$1] }
| predicate COMMA  predicate_list  { $1 :: $3 }

term:

| term PLUS term { TermExp("plus", [$1;$3]) }
| term MULTIPLY term { TermExp("multiply", [$1;$3]) }
| term MINUS term { TermExp("minus", [$1;$3]) }
| constant  { Constant $1 }
| VARIABLE {Variable $1}
| predicate  { $1 }
| list {$1}

list :
| LEFTSQUARE RIGHTSQUARE {TermExp("empty_list",[])}
| LEFTSQUARE list_body  RIGHTSQUARE {$2}

list_body :
| term { TermExp("list", [$1;TermExp("empty_list",[])]) }
| term COMMA list_body { TermExp("list", [$1;$3]) }
| term DANDA term  { TermExp("list", [$1;$3]) }

term_list:
| term { [$1] }
| term COMMA term_list { $1 :: $3 }

constant:
| INT  { Int $1 }
| ATOM  { String $1 }


