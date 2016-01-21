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
    TypeFloat {TokenTypeF}
    TypeInt {TokenTypeI}
    TypeString {TokenTypeS}
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
--Could I make this into lists???/

--Have one rule that is 

Program : DeclarationsList StatementList { Program $1 $2}

DeclarationsList : Declaration { DeclarationsList DEmpty $1}
                 | DeclarationsList Declaration {DeclarationsList $1 $2} 

Declaration : var id ':' TypeInt ';' {DeclarationInt $2}
            | var id ':' TypeFloat  ';' {DeclarationFloat $2 } 
            | var id ':' TypeString ';' {DeclarationString $2 } 

StatementList : Statement {StatementList SEmpty $1} 
            | StatementList Statement {StatementList $1 $2}

Statement : 'if' id 'then' StatementList endif {IfState $2 $4}
            |'if' id 'then' StatementList 'else' StatementList endif {IfElseState $2 $4 $6} 
            |while id do StatementList done {WhileState $2 $4}
            | print Line ';' {PrintState $2}
            | read Line ';' {ReadState $2}
            |id '=' Exp ';' {AssnStatement $1 $3}--problem 

NUM : int {NumInt $1}
    | float {NumFloat $1}

Line : '"' string '"'  {StringLit $2}
    |Exp {StringExp $1}
--    |id {StringId $2} --THIS RULE IS BRoKEN

           
Exp :  Exp '*' Exp {Mult $1 $3}--Ints
    | Exp '+' Exp {Plus $1 $3}
    | Exp '-' Exp {Subt $1 $3}
    | Exp '/' Exp {Divi $1 $3}
    | '-' Exp %prec UMINUS {Neg $2}
    | NUM {NUM $1}
    | id {Id $1}

{
parseError :: [Token] -> a
parseError _ = error "Parse Error"
}
