{
module a1Scanner (main) where
}

%wrapper "basic"

tokens :-
    "#".                                    ;
    [\+\*\-\/]                              {\s->Op (head s)}        
    "var"                                   { \s -> Id } --Here we extract the id from the variable.
    "="                                     { \s -> Equal }
    (((([1-9]+[0-9]*)|(0))?)(\.))(([0-9]*)) { \s -> (read s) } --I want x., .y, or x.y
    ([1-9][0-9]*)|(0)                      { \s -> (read s) }
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
