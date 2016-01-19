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
    'if'      { TokenIf }
    'then'    { TokenThen }
    'else'    { TokenElse }
    endif   { TokenEndif }
    while   { TokenWhile }
    done    { TokenDone }
    read    { TokenRead }
    print   { TokenPrint }
    id      { TokenID $$ }
    var     { TokenVar }
    ';'     { TokenSemicolon }
%%
--This is where I define my grammar.
--statements

Program : StatementList ';'

Statement : 'if' id 'then' StatementList endif {$2}
            |'if' id 'then' StatementList 'else' StatementList endif {#2}
            |'while' id 'do' StatementList 'done' {$2}
            | print '"'Exp'"'
            | read '"' Exp '"'
            | Assignment

StatementList : Statement 
            | StatementList ';' Statement

Assignment :  id '=' Exp {$1}
            | var id ':' int {$2} 
            | var id ':' float {$2}

Exp : '"' Exp '"' { String $2 }
    | Exp '*' Exp {Mult $1 $3}
    | Exp '+' Exp {Plus $1 $3}
    | Exp '-' Exp {Subt $1 $3}
    | Exp '/' Exp {Divi $1 $3}
    | '-' Exp %prec UMINUS {Neg $2}
    | id {$1}

--data Program =  
  --  deriving Show

data Exp = 
    
    deriving Show

data Assignment = 
    deriving Show

data Statement = 
    deriving Show

data StatementList =
    deriving Show

data Assignment =

    deriving Show

{
parseError :: [Token] -> a
parseError _ = error 'Parse error'
}
