{
module A1Lexer  where
--Make this (main) when you want to print it off
}

%wrapper "basic"

tokens :-
    $white+                                 ;
    ":"                                     {\s->TokenColon}
    "#".*                                   ;
   -- [\+\*\-\/]                              {\s->TokenOp (head s)}        
    "+"                                     {\s->TokenPlus}
    "-"                                     {\s ->TokenMinus}
    "/"                                     {\s->TokenDiv}
    "*"                                     {\s->TokenMult}
    "="                                     { \s -> TokenEqual }
    (((([1-9]+[0-9]*)|(0))?)(\.))(([0-9]*)) { \s -> TokenFloat (read s) } --I want x., .y, or x.y
    ([1-9][0-9]*)|(0)                       {\s -> TokenInt (read s) }
    ";"                                     {\s->TokenSemicolon}             
    "if"                                    { \s-> TokenIf }
    "else"                                  { \s -> TokenElse }
    "then"                                  { \s-> TokenThen}
    "endif"                                 { \s -> TokenEndIf }
    "while"                                 { \s -> TokenWhile }
    "done"                                  { \s -> TokenDone }
    "do"                                    {\s->TokenDo}
    "int"                                   { \s-> TokenTypeI }
    "float"                                 { \s-> TokenTypeF}
    "string"                                {\s->TokenTypeS}
    "print"                                 { \s -> TokenPrint }
    "read"                                  { \s -> TokenRead }
    "var"                                   { \s -> TokenVar} 
    """                                     {\s->TokenQuote}
    "\[a-zA-Z0-9\.\!\?' ']+\"                  { \s-> TokenString s } --HERE IS A PROBLEM
    [a-zA-Z0-9]+                            { \s -> TokenId  s} --Here we extract the id from the variable.

    
-- Each action has type :: String -> Token

{
data Token = 
    TokenInt Int     |
    TokenFloat Float |
    TokenString String|
    TokenSemicolon   |
    TokenColon      |
    TokenId String   | 
    TokenIf          |
    TokenThen       |
    TokenExpr        |
    TokenEndIf       |
    TokenElse        |
    TokenWhile       |
    TokenDone        |
    TokenDo         |
    TokenVar         |
    TokenQuote      |
    TokenTypeI       |
    TokenTypeF       |
    TokenTypeS      |
    TokenPlus       |
    TokenMinus      |
    TokenMult       |
    TokenDiv        |
    --TokenOp Char     |
    TokenEqual       |
    TokenRead       |
    TokenPrint       
    deriving(Eq,Show)

--main = do
--    s <- getContents
--    print (alexScanTokens s)
}
