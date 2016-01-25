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


--Data Structures
data Token = 
        TokenInt  Int           |
        TokenFloat  Float       |
        TokenString  String     |
        TokenSemicolon          |
        TokenColon              |
        TokenId  String         | 
        TokenIf                 |
        TokenThen               |
        TokenExpr               |
        TokenEndIf              |
        TokenElse               |
        TokenWhile              |
        TokenDone               |
        TokenDo                 |
        TokenVar                |
        TokenQuote              |
        TokenTypeI              |
        TokenTypeF              |
        TokenTypeS              |
        TokenPlus               |
        TokenMinus              |
        TokenMult               |
        TokenDiv                |
        TokenEqual              |
        TokenRead               |
        TokenPrint              |
        EOFToken                
        deriving(Eq,Show)


data BinOp = Add | Multiply | Subtract | Divide deriving(Eq,Show)

--data UnOp = Neg deriving(Eq,Show)

type Id = String 

--data Id =  String deriving(Eq,Show)

data LineStmt = StringLine String | ExprString Expr deriving(Eq,Show)

data Decl = DecSeq [Decl] | Dec Id Type  deriving(Eq,Show)

data Assn = Assn String Expr  deriving(Eq,Show)

data Stmt = Seq [Stmt] | If Id Stmt | IfElse Id Stmt Stmt | While Id Stmt | Read LineStmt | Print LineStmt | Expr |IdStmt String deriving(Eq,Show)

data Expr = Var String | IntConst Integer | FloatConst Double | Binary BinOp Expr Expr | Neg Expr  deriving(Eq,Show)

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


miniParser:: Parser Stmt
miniParser = whiteSpace >> declaration >> statement

statement::Parser Stmt 
statement = sequenceOfStmt <|> statement


sequenceOfStmt = do 
        list <- (sepBy1 statement' semi) 
        let res =  if (length list == 1) then (head list) else Seq list
        return res

declaration :: Parser Decl
declaration = sequenceOfDecl <|> declaration

sequenceOfDecl = do 
    list <- (sepBy1 declaration' semi)
    let res = if length list == 1 then head list else DecSeq list
	in
        return res

statement' :: Parser Stmt
statement' = ifStmt
    <|> ifElseStmt
    <|> whileStmt
    <|> idStmt
    -- <|> printStmt
-- <|> readStmt
           
declaration' :: Parser Decl
declaration' = decStmt

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

assignStmt :: Parser Assn 
assignStmt =
  do reserved "var"
     var  <- identifier
     reservedOp "="
     expr <- expression
     semi
     return $ Assn var expr

--NEED TO DO
idStmt::Parser Stmt
idStmt = do
    ident <- identifier
    return $  IdStmt ident

{-
line:: Parser LineStmt
line = stringLit <|>expression

readStmt:: Parser Stmt --get line and return it
readStmt = do 
    reserved "read"
    str <- expression 
    semi
    return $ Read str

printStmt ::Parser Stmt
printStmt = do
    reserved "write"
    str <- expression --an identifier or a string
    semi
    return $ Print str
-}

typer :: Parser Type
typer = stringType <|> intType <|> floatType

stringType:: Parser Type
stringType = do
    val <- stringLit
    return $ StringType "string"


floatType:: Parser Type
floatType = do
    val <- float
    return $ FloatType "float"

intType:: Parser Type
intType = do
    val <- integer
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
term =  expression
    <|> liftM Var identifier
    <|> liftM IntConst integer
    <|> liftM FloatConst float


parseString :: String -> Stmt
parseString str =
  case parse miniParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
  do program  <- readFile file
     case parse miniParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
