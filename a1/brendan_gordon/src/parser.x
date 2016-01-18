{
module Main (main) where
}

%wrapper "basic"

tokens :-
    $white+                                 ;
    "#".*                                   ;
    [\+\*\-\/]                              {\s->Op (head s)}        
    "="                                     { \s -> Equal }
    (((([1-9]+[0-9]*)|(0))?)(\.))(([0-9]*)) { \s -> Float (read s) } --I want x., .y, or x.y
    ([1-9][0-9]*)|(0)                       {\s -> Int (read s) }
    ";"                                     {\s->Semicolon}             
    "if"                                    { \s-> If }
    "else"                                  { \s -> Else }
    "endif"                                 { \s -> EndIf }
    "while"                                 { \s -> While }
    "done"                                  { \s -> Done }
    "int"                                   { \s-> TypeI }
    "float"                                 { \s-> TypeF}
    "read"                                  { \s -> Read' }
    "print"                                 { \s -> Print }
    [a-zA-Z0-9]+                            { \s -> Id  s} --Here we extract the id from the variable.
    
-- Each action has type :: String -> Token

{
data Token = 
    Int Int     |
    Float Float |
    String      |
    Semicolon   |
    Id String   | 
    If          |
    Expr        |
    EndIf       |
    Else        |
    While       |
    Done        |
    TypeI       |
    TypeF       |
    Op Char     |
    Equal       |
    Read'       |
    Print       
    deriving(Eq,Show)

main = do
    s <- getContents
    print (alexScanTokens s)

}
