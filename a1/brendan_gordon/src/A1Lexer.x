{
module A1Lexer  where
--Make this (main) when you want to print it off
 }

%wrapper "posn"

tokens :-
    $white+                                 ;
    ":"                                     {\p s->TokenColon p}
    "#".*                                   ;
   -- [\+\*\-\/]                              {\p s->TokenOp p (head s) }        
    "+"                                     {\p s->TokenPlus p}
    "-"                                     {\p s ->TokenMinus p}
    "/"                                     {\p s->TokenDiv p}
    "*"                                     {\p s->TokenMult p}
    "="                                     {\p s -> TokenEqual  p}
    (((([1-9]+[0-9]*)AlexPosn|(0))?)(\.))(([0-9]*)) {\p s -> TokenFloat p (read s)  } --I want x., .y, or x.y
    ([1-9][0-9]*)AlexPosn|(0)                       {\p s -> TokenInt p (read s)  }
    ";"                                     {\p s->TokenSemicolon p}             
    "if"                                    {\p s-> TokenIf  p}
    "else"                                  {\p s -> TokenElse  p}
    "then"                                  {\p s-> TokenThen p}
    "endif"                                 {\p s -> TokenEndIf  p}
    "while"                                 {\p s -> TokenWhile  p}
    "done"                                  {\p s -> TokenDone  p}
    "do"                                    {\p s->TokenDo p}
    "int"                                   {\p s-> TokenTypeI  p}
    "float"                                 {\p s-> TokenTypeF p}
    "string"                                {\p s->TokenTypeS p}
    "print"                                 {\p s -> TokenPrint  p}
    "read"                                  {\p s -> TokenRead  p}
    "var"                                   {\p s -> TokenVar p} 
    --"""                                    {\p s->TokenQuote p}
    \"[a-zA-Z0-9\.\!\?' ']+\"               {\p s-> TokenString p s} --HERE IS A PROBLEM
    [a-zA-Z0-9]+                            {\p s -> TokenId p s } --Here we extract the id from the variable.

    
-- Each action has type :: String -> Token


{

tok f p s = f p s

data Token = 
    TokenInt AlexPosn Int |
    TokenFloat AlexPosn Float |
    TokenString AlexPosn String|
    TokenSemicolon  AlexPosn|
    TokenColon AlexPosn|
    TokenId AlexPosn String| 
    TokenIf AlexPosn|
    TokenThen AlexPosn|
    TokenExpr AlexPosn|
    TokenEndIf AlexPosn|
    TokenElse AlexPosn|
    TokenWhile AlexPosn|
    TokenDone AlexPosn|
    TokenDo AlexPosn|
    TokenVar  AlexPosn|
    TokenQuote AlexPosn|
    TokenTypeI AlexPosn|
    TokenTypeF AlexPosn|
    TokenTypeS AlexPosn|
    TokenPlus  AlexPosn|
    TokenMinus AlexPosn|
    TokenMult  AlexPosn|
    TokenDiv   AlexPosn|
    --TokenOp Char     AlexPosn|
    TokenEqual AlexPosn|
    TokenRead  AlexPosn|
    TokenPrint AlexPosn      
    deriving(Eq,Show)


--main = do
  --  s <- getContents
    --print (alexScanTokens s)

 }

