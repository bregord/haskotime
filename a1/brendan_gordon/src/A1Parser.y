{
module A1Parser where
import A1Lexer
import Types
}

%name calc--name of parsing function Happy generates
%tokentype { Token }--type of tokens the parser accepts
%error { parseError }--function Happy calls in event of parser error
--define precedences here. earlier is lower precedence
%left '+' '-'
%left '*' '/'
%left UMINUS

%token
    '+'     { TokenPlus }
    '*'     { TokenMult }
    '-'     { TokenMinus }
    '/'     { TokenDiv }
    '='     { TokenEqual }
    '"'     { TokenQuote }
    int    { TokenInt $$ }
    float   { TokenFloat $$ }
    string { TokenString $$ }
    'if'      { TokenIf }
    'then'    { TokenThen }
    'else'    { TokenElse }
    endif   { TokenEndIf }
    while   { TokenWhile }
    done    { TokenDone }
    do    { TokenDo }
    read    { TokenRead }
    print   { TokenPrint }
    id      { TokenId $$ }
    var     { TokenVar }
    ';'     { TokenSemicolon }
    ':'     { TokenColon }
%%

--This is where I define my grammar.
--statements

Program : DeclarationsList ';' StatementList ';' { Program $1 $3}

DeclarationsList : Declaration { DeclarationsList DEmpty $1}
                 | DeclarationsList ';' Declaration ';' {DeclarationsList $1 $3} 
                    |   {DEmpty}

Declaration : var id ':' int {DeclarationInt $2 $4}
            | var id ':' float {DeclarationFloat $2 $4} 
            | var id ':' string {DeclarationString $2 $4} 
Statement : 'if' id 'then' StatementList endif {IfState $2 $4}
            |'if' id 'then' StatementList 'else' StatementList endif {IfElseState $2 $4 $6} 
            |while id do StatementList done {WhileState $2 $4}
            | print '"' Line '"' {PrintState $3}
            | read '"' Line '"'  {ReadState $3}
            |id '=' Exp {AssnStatement $1 $3}--problem 

StatementList : Statement {StatementList SEmpty $1} 
            | StatementList ';' Statement {StatementList $1 $3}
            |   {SEmpty}

--REMEMBER. NO MULTIPLE ASSIGNMENTS. THIS GRAMMAR CURRENTLY ALLWOS IT.
 
NUM : int {NumInt $1}
    | float {NumFloat $1}

Line : string  {StringLit $1}
    |Exp {StringExp $1}

           
Exp :  Exp '*' Exp {Mult $1 $3}--Ints
    | Exp '+' Exp {Plus $1 $3}
    | Exp '-' Exp {Subt $1 $3}
    | Exp '/' Exp {Divi $1 $3}
    | '-' Exp %prec UMINUS {Neg $2}
    | NUM {NUM $1}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
