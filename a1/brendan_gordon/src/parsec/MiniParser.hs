module MiniParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

--Grammar
{-program ::= declarationsList statementsList | [] statementsList | declarationsList []

declarationsList ::= declaration | declarationsList declaration

declaration ::= var id colon intType semicolon | var id colon floatType semicolon | var id colon stringType semicolon 

statementsList ::= statement | statementsList statement

statement ::= if id then statementsList | if id then statementsList else statementsList endif | while id do statementsList done | print line | read line | id = exp

line ::= string | exp

exp ::= exp * exp | exp + exp | exp - exp | exp / exp | - exp | int | float | id -}


data BinOp = Add | Multiply | Subtract | Divide deriving(Eq,Show)

--data UnOp = Neg deriving(Eq,Show)

type Id = String 

--data Id =  String deriving(Eq,Show)

data LineStmt = StringLine String | IdString Id deriving(Eq,Show)

data Decl = DecSeq [Decl] | Dec Id Type  deriving(Eq,Show)

data Stmt = Seq [Stmt] | If Id Stmt | IfElse Id Stmt Stmt | While Id Stmt | Read Id | Print Id | Expr |IdStmt String | Assn Id Expr deriving(Eq,Show)

data Expr = Var String | IntConst Integer | FloatConst Double| Binary BinOp Expr Expr | Neg Expr  deriving(Eq,Show)

data Type = FloatType String | IntType String | StringType String deriving (Eq,Show)


languageDef = 
    emptyDef{
           Token.commentLine     = "#"
           , Token.identStart = letter
           ,Token.identLetter = alphaNum
          , Token.reservedNames   = [ "if"
                                    , "then"
                                    , "else"
                                    , "while"
                                    , "do"
                                    , "done"
                                    , "endif"
                                    , "var"
                                    , "read"
                                    ,"print"
                                    ]
          , Token.reservedOpNames = ["+", "-", "*", "/", "="
          ]
    }


lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
stringLit = Token.stringLiteral lexer
integer  = Token.integer lexer -- parses an integer
float = Token.float lexer
semi      = Token.semi       lexer -- parses a semicolon
colon       =Token.colon       lexer
whiteSpace = Token.whiteSpace lexer -- parses whitespace
parens = Token.parens lexer
dot = Token.dot lexer

miniParser:: Parser [Stmt]
miniParser = whiteSpace >> many declaration >> many statement

statement :: Parser Stmt
statement = ifStmt
    <|> ifElseStmt
    <|> whileStmt
    <|> assignStmt
    <|> printStmt
    <|> readStmt
          
declaration :: Parser Decl
declaration = decStmt

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <-identifier 
     reserved "then"
     stmt1 <- statement
     return $ If cond stmt1 


ifElseStmt :: Parser Stmt
ifElseStmt =
  do reserved "if"
     cond  <- identifier 
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ IfElse cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- identifier 
     reserved "do"
     stmt <- statement
     reserved "done"
     return $ While cond stmt

assignStmt :: Parser Stmt 
assignStmt =
  do var  <- identifier
     reservedOp "="
     expr <- expression
     semi
     return $ Assn var expr

--NEED TO DO
idStmt::Parser Stmt
idStmt = do
    ident <- identifier
    return $  IdStmt ident

readStmt:: Parser Stmt --get line and return it
readStmt = do 
    reserved "read"
    str <- identifier 
    semi
    return $ Read str

printStmt ::Parser Stmt
printStmt = do
    reserved "print"
    str <- identifier --an identifier or a string
    semi
    return $ Print str

typer :: Parser Type
typer = stringType <|> intType <|> floatType

stringType:: Parser Type
stringType = do
    val <- reserved "string"
    return $ StringType "string"

floatType:: Parser Type
floatType = do
    val <- reserved "float"
    return $ FloatType "float"

intType:: Parser Type
intType = do
    val <- reserved "int"
    return $ IntType "int"

decStmt :: Parser Decl
decStmt = 
  do 
    reserved "var"
    var  <- identifier
    colon
    typ <- typer--It's a string, float, or integer
    semi
    return $ Dec var typ

expression :: Parser Expr
expression = buildExpressionParser operators term

operators = [ [Prefix (reservedOp "-" >> return (Neg             ))          ]
             , [Infix  (reservedOp "*"   >> return (Binary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (Binary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (Binary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (Binary Subtract)) AssocLeft]
              ]

--THIS PART

floatParser:: Parser Double 
floatParser = 
    do
        pre <- many digit
        dot
        post <- many digit
        case (pre,post) of
            ([],[]) -> fail "Not a number"--Fail
            (n,[]) -> return $ read n
            ([],m) -> return $ read m
            (n,m) -> return $ (read $ (n ++ "." ++  m)) --map show over both lists. Then concatenate it and read it.

term::Parser Expr 
term = parens expression  
    <|>liftM Var identifier
    <|> try ( liftM FloatConst floatParser) 
    <|> (liftM IntConst integer)
    -- <|> liftM FloatConst float --FloatConst Double 
    --get as a list of numbers, and just zip with increasing powers of 10 to actually make it a number


parseString :: String -> [Stmt]
parseString str =
  case parse (miniParser  <* eof) "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO [Stmt]
parseFile file =
  do program  <- readFile file
     case parse (miniParser <* eof) file program   of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
