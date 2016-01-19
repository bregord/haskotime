{
module main where
import lexer
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
    '='     { TokenEq }
    '"'     { TokenQuote }
    int    { TokenInt $$ }
    float   { TokenFloat $$ }
    string { TokenString $$ }
    'if'      { TokenIf }
    'then'    { TokenThen }
    'else'    { TokenElse }
    endif   { TokenEndif }
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

DeclarationsList : Declaration { DeclarationsList $1}
                 | DeclarationsList ';' Declaration ';' {DeclarationsList $1 3} 

Declaration : var id ':' int {Id $2} 
            | var id ':' float {Id $2}
            | var id ':' string {Id $2}

Statement : 'if' id 'then' StatementList endif {Id $2}
            |'if' id 'then' StatementList 'else' StatementList endif {Id #2}
            |while id do StatementList done {Id $2}
            | print '"' Exp '"' {Statement $3} 
            | read '"' Exp '"' {Statement $3}
            | Assignment {$1}   

StatementList : Statement {$1} 
            | StatementList ';' Statement {StatementList $1 $3}

--REMEMBER. NO MULTIPLE ASSIGNMENTS. THIS GRAMMAR CURRENTLY ALLWOS IT.
Assignment :  id '=' Exp {Id $1}
            
Exp : '"' Exp '"' {String $2 }
    | Exp '*' Exp {Mult $1 $3}--Ints
    | Exp '+' Exp {Plus $1 $3}
    | Exp '-' Exp {Subt $1 $3}
    | Exp '/' Exp {Divi $1 $3}
    | '-' Exp %prec UMINUS {Neg $2}
    | id {$1}

{-
data Exp = String String
    |Mult Int Int
    |Plus Int Int
    |Subt Int Int
    |Divi Int Int
    |Neg Int
    |Id Int
    |Mult Float Float
    |Plus Float Float 
    |Subt Float Float 
    |Divi Float Float 
    |Neg Float Float 
    |Id Float 
    Interiving Show

data Declaration = Int Int
                |Float Float
                |String String

data Statement = Id Exp
                |Id Exp
                |Id Exp
               
              
    deriving Show

data StatementList =
    deriving Show

data Assignment =

    deriving Show
-}
{
parseError :: [Token] -> a
parseError _ = error 'Parse error'
}
