{
module A1Lexer  where
--Make this (main) when you want to print it off
}

%wrapper "basic"

tokens :-
    $white+                                 ;
    ":"                                     ;
    "#".*                                   ;
    [\+\*\-\/]                              {\s->TokenOp (head s)}        
    "="                                     { \s -> TokenEqual }
    (((([1-9]+[0-9]*)|(0))?)(\.))(([0-9]*)) { \s -> TokenFloat (read s) } --I want x., .y, or x.y
    ([1-9][0-9]*)|(0)                       {\s -> TokenInt (read s) }
    ";"                                     {\s->TokenSemicolon}             
    "if"                                    { \s-> TokenIf }
    "else"                                  { \s -> TokenElse }
    "endif"                                 { \s -> TokenEndIf }
    "while"                                 { \s -> TokenWhile }
    "done"                                  { \s -> TokenDone }
    "int"                                   { \s-> TokenTypeI }
    "float"                                 { \s-> TokenTypeF}
    "print"                                 { \s -> TokenPrint }
    "read"                                  { \s -> TokenRead' }
    "var"                                   { \s -> TokenVar} 
    \"[a-zA-Z0-9\.\!\?' ']*\"               { \s-> TokenString s }
    [a-zA-Z0-9]+                            { \s -> TokenId  s} --Here we extract the id from the variable.

    
-- Each action has type :: String -> Token

{
data Token = 
    TokenInt Int     |
    TokenFloat Float |
    TokenString String|
    TokenSemicolon   |
    TokenId String   | 
    TokenIf          |
    TokenExpr        |
    TokenEndIf       |
    TokenElse        |
    TokenWhile       |
    TokenDone        |
    TokenVar         |
    TokenTypeI       |
    TokenTypeF       |
    TokenOp Char     |
    TokenEqual       |
    TokenRead'       |
    TokenPrint       
    deriving(Eq,Show)

--main = do
--    s <- getContents
--    print (alexScanTokens s)
}
