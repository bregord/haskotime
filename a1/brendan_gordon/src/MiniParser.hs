module Main where

import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Token as Token

data BinOp = Add | Multiply | Subtract | Divide deriving(Eq,Show)

type Id = String 

data LineStmt = StringLine String | IdString Id deriving(Eq,Show)

data Decl = DecSeq [Decl] | Dec Id Type  deriving(Eq,Show)

data Stmt = Seq [Stmt] | If Expr [Stmt] | IfElse Expr [Stmt] [Stmt] | While Expr [Stmt] | Read Expr | Print Expr | Expr |IdStmt String | Assn Id Expr deriving(Eq,Show)

data Expr = Var String | IntConst Integer | FloatConst Double| Binary BinOp Expr Expr | Neg Expr | StringEx String  deriving(Eq,Show)

data Type = FloatType| IntType | StringType | NullType deriving (Eq,Show)

data Program = Program [Decl] [Stmt] deriving(Eq,Show)

data AssocMap = AssocMap (Id, Type) deriving(Eq,Show)

data CheckError = CheckError String deriving(Eq, Show)

languageDef = 
    emptyDef{
           Token.commentLine     = "#"
           , Token.identStart = letter <|> char '_'
           ,Token.identLetter = alphaNum <|> char '_'
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
                                    , "int"
                                    , "float"
                                    , "string"
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


miniParser:: Parser Program
miniParser = 
    do 
        whiteSpace 
        x <- many declaration 
        y <- many statement
        return $ Program  x y

--miniParser:: Parser [Stmt]
--miniParser = whiteSpace >> many declaration >> many statement
--Compose miniParser with another to get declarations too.

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
     cond  <-expression
     reserved "then"
     stmt1 <- many statement
     reserved "endif"
     return $ If cond stmt1 


ifElseStmt :: Parser Stmt
ifElseStmt =
  do reserved "if"
     cond  <- expression 
     reserved "then"
     stmt1 <- many statement
     reserved "else"
     stmt2 <- many statement
     return $ IfElse cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- expression 
     reserved "do"
     stmt <- many statement
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
    str <- expression 
    semi
    return $ Read str

printStmt ::Parser Stmt
printStmt = do
    reserved "print"
    str <- expression --an identifier or a string
    semi
    return $ Print str

typer :: Parser Type
typer = stringType <|> intType <|> floatType

stringType:: Parser Type
stringType = do
    val <- reserved "string"
    return $ StringType

floatType:: Parser Type
floatType = do
    val <- reserved "float"
    return $ FloatType 

intType:: Parser Type
intType = do
    val <- reserved "int"
    return $ IntType 

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

floatParser:: Parser Double 
floatParser = 
    do
        pre <- many digit
        dot
        post <- many digit
        case (pre,post) of
            ([],[]) -> fail "Not a number"--Fail
            (n,[]) ->  if (head n) == '0' && (length n) > 1 then fail "0 error"  else return $ read n 
            ([],m) -> return $ read m
            (n,m) -> if (head n) == '0' && (length n) > 1 then fail "0 error" else return $ (read $ (n ++ "." ++  m)) --map show over both lists. Then concatenate it and read it.

integerParser:: Parser Integer
integerParser = 
    do
        num <-many digit
        --return $ read num
        if (head num) == '0' && (length num) > 1 then fail "0 error"  else return $ read num
       -- case num of
        --0:[] -> return 0
        --0:_ -> fail
        --[_] -> return num

letters = ['0'..'9']++['a'..'z']++['A'..'Z']++[',','.','!','!',' ']

stringParser:: Parser String
stringParser = 
    do
        char '"'
        str <- many $ oneOf letters 
        char '"'
        return $ str

term::Parser Expr 
term = parens expression  
    <|>liftM Var identifier
    <|> try ( liftM FloatConst floatParser) 
    <|> try (liftM IntConst integerParser)
    <|> liftM StringEx stringParser
    -- <|> liftM FloatConst float --FloatConst Double 
    --get as a list of numbers, and just zip with increasing powers of 10 to actually make it a number


--Pretty Printer
class PrettyPrint a where
    prettyPrint::a->String

instance PrettyPrint Expr where
    prettyPrint (Neg a) = ("-" ++ (prettyPrint a)) 
    prettyPrint (Var a) =  a
    prettyPrint (IntConst a) = show a
    prettyPrint (FloatConst a) = show a
    prettyPrint (StringEx a) = a
    prettyPrint (Binary op a b) =  read $ (prettyPrint a) ++ (prettyPrint op) ++ (prettyPrint b)

instance PrettyPrint Stmt where
    prettyPrint (Seq a) = prettyPrint a
    prettyPrint (If a b) =  "if " ++ (prettyPrint a) ++ " then " ++ (prettyPrint b) 
    prettyPrint (IfElse a b c) = "if " ++ (prettyPrint a) ++ " then " ++ (prettyPrint b) ++ " else " ++ (prettyPrint c)
    prettyPrint (While a b) =  "while " ++ (prettyPrint a) ++ " do " ++ (prettyPrint b) ++ " done" 
    prettyPrint (Print a) = prettyPrint a ++ ";"
    prettyPrint (Read a) =  prettyPrint a ++ ";"
    prettyPrint (Assn a b) =  a ++ " = " ++  (prettyPrint b) ++ ";"
    prettyPrint (IdStmt a) = a

instance PrettyPrint a => (PrettyPrint [a]) where
    prettyPrint a = concat $ map prettyPrint a

instance PrettyPrint BinOp where
    prettyPrint Add = "+"
    prettyPrint Multiply = "*"
    prettyPrint Subtract = "-"
    prettyPrint Divide = "/"

instance PrettyPrint Decl where
    prettyPrint (DecSeq a) = prettyPrint a
    prettyPrint (Dec a b) =  "var " ++ (a) ++ ": " ++ prettyPrint b ++ ";"

instance PrettyPrint Type where
    prettyPrint (FloatType) = "float"
    prettyPrint (IntType) = "int"
    prettyPrint (StringType) = "string" 

instance PrettyPrint Program where
    prettyPrint (Program a b) = (prettyPrint a) ++ (prettyPrint b)

instance PrettyPrint AssocMap where
    prettyPrint (AssocMap (a, b)) = a ++ ":" ++ (prettyPrint b) ++ "\n" 

pretty::(PrettyPrint a)=>a->String
pretty a = prettyPrint a

parseString :: String -> Program
parseString str =
  case parse (miniParser  <* eof) "" str of
    Left e  -> error $ show e
    Right r ->  r

parseFile :: String -> IO () 
parseFile file =
  do program  <- readFile file
     case parse (miniParser <* eof) file program   of
       Left e -> do putStrLn "Invalid" >> print e 
       Right r -> do putStrLn "Valid" >> print r


--Type Checking
symAdd::Decl->AssocMap
symAdd (Dec a b) = AssocMap(a,b)
--if a stmt is used in a different context than it should be, return false. otherwise true.
--checkValidity::[AssocMap]->[Stmt]->Bool

--design. maybe i ma


--Want to make sure types match up.
typeCheckStmt::[AssocMap]->Stmt->Bool
typeCheckStmt [] _ =  False --ERROR - Clearly the thing is not declared 
typeCheckStmt m (Assn a b) = if (isInMap m a) && (typeCheckExpr m b) == getTypeFromMap m a then True else False 
typeCheckStmt m (If a b)  = if  (typeCheckExpr m a) == IntType && typeCheckStmtList m b then True else False 
typeCheckStmt m (IfElse a b c) = if (typeCheckExpr m a)==IntType && typeCheckStmtList m b   then True else False 
typeCheckStmt m (While a b) = if (typeCheckExpr m a)==IntType && typeCheckStmtList m b  then True else False
--typeCheckStmt m (Expr) =  
--typeCheckStmt m (Read a) = if 
--typeCheckStmt m (Print a) = 
--typeCheckStmt m (IdStmt a) = 

typeCheckStmtList::[AssocMap]->[Stmt]->Bool
typeCheckStmtList m [x] = typeCheckStmt m x
typeCheckStmtList m (x:xs) = (typeCheckStmt m x) && (typeCheckStmtList m xs)


typeCheckExpr::[AssocMap]->Expr->Type
typeCheckExpr m (Var a) = getTypeFromMap m a --get from the list and return that 
typeCheckExpr m (IntConst a) = IntType
typeCheckExpr m (FloatConst a) = FloatType
typeCheckExpr m (Binary op a b) = typeCheckBinOp m op a b
typeCheckExpr m (Neg a) = typeCheckExpr m a 
typeCheckExpr m (StringEx a) = StringType

--this is where most rules come into play

--DO AN EITHER STATEMENT HERE
typeCheckBinOp::[AssocMap]->BinOp->Expr->Expr->Type
typeCheckBinOp m op lt rt =
    do 
        lhs <- ( typeCheckExpr m lt)
        rhs <- ( typeCheckExpr m rt)
        case (op, lhs,rhs) of
			(Add, IntType, IntType) ->return  IntType 
			(Add, IntType, FloatType)->return  FloatType 
			(Add, FloatType, IntType)->return  FloatType 
			(Add, FloatType, FloatType)->return  FloatType 
			(Add, StringType, StringType)->return  StringType 
			(Subtract, IntType, IntType) ->return  IntType 
			(Subtract, IntType, FloatType) ->return  FloatType 
			(Subtract, FloatType, IntType) ->return  FloatType 
			(Subtract, FloatType, FloatType) ->return  FloatType 
			(Subtract, StringType, StringType) ->return  StringType
			(Divide, IntType, IntType) ->return  IntType 
			(Divide, IntType, FloatType) ->return  FloatType 
			(Divide, FloatType, IntType) ->return  FloatType 
			(Divide, FloatType, FloatType) ->return  FloatType 
			(Multiply, IntType, IntType) ->return  IntType 
			(Multiply, IntType, FloatType) ->return  FloatType 
			(Multiply, FloatType, IntType) ->return  FloatType 
			(Multiply, FloatType, FloatType) -> return FloatType 
			(_,_,_) ->return NullType 

getTypeFromMap::[AssocMap]->Id->Type
getTypeFromMap [] a = NullType--Or maybe it should fail 
getTypeFromMap (AssocMap (i,t):xs) a = if i == a then t else (getTypeFromMap xs a)


--ERROR HANDLING
isInMap::[AssocMap]->Id->Bool
isInMap [] _ = False
isInMap (AssocMap (i,t):xs) a = if  i ==a then True else isInMap xs a



typeCheck::Program->String->IO Bool
typeCheck (Program a b) fileName = 
    do
        --let handle = (many (noneOf "." fileName)) -- ++ ".symbol.txt"
        handle <- openFile (fileName ++ ".symbol.txt") WriteMode
        let assocMapList = map symAdd a --list of maps
        --
        let b = map pretty assocMapList
        hPutStr handle $ concat b
        hClose handle
        return True

main = do
    (arg:_) <- getArgs 
    parseFile arg
