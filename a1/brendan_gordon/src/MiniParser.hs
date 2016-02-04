{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.FilePath
import System.Exit
import System.Environment
import System.IO
import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Token as Token

--The parameter a refers to a the "annotation" of a particular structure. This is primarily for the codegen. During typechecking, it will allow us to know the type of any particular statement or expression.
data BinOp = Add | Multiply | Subtract | Divide deriving(Eq,Show)

data Id a = Val String a deriving(Eq, Show,Functor,Ord)

data Decl = DecSeq [Decl] | Dec String Type  deriving(Eq,Show)

data Stmt a = Seq [Stmt a] | If (Expr a) [Stmt a]| IfElse (Expr a) [Stmt a] [Stmt a]| While (Expr a) [Stmt a] | Read (Id a)| Print (Expr a)|IdStmt String| Assn (Id a) (Expr a) deriving(Eq,Show)

data Expr a = Var a String  | IntConst a Integer| FloatConst a Double | Binary a BinOp (Expr a) (Expr a) | Neg a (Expr a)| StringEx  a String deriving(Eq,Show)

data Type = FloatType| IntType | StringType | Void deriving (Eq,Show)

data Program a = Program [Decl] [Stmt a] deriving(Eq,Show)

data Annotated a x = Ann a x

type TypeAnnotated x = Annotated Type x

data AssocMap = AssocMap(String, Type) deriving(Eq,Show)

--data AssocMap = AssocMap (M.Map Id Type) deriving Eq, Show

data SemError = CheckError String | IncompatibleTypes String | NoDecs String | ReDecs String 
 | ProgramError String deriving(Eq, Show)

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

miniParser:: Parser (Program ())
miniParser = 
    do 
        whiteSpace 
        x <- many declaration 
        y <- many statement
        return $ Program x y

statement :: Parser (Stmt ())
statement = ifElseStmt
    <|>  ifStmt
    <|> whileStmt
    <|> assignStmt
    <|> printStmt
    <|> readStmt
          
declaration :: Parser Decl
declaration = decStmt

ifStmt :: Parser (Stmt ())
ifStmt =
  do reserved "if"
     cond  <-expression
     reserved "then"
     stmt1 <- many statement
     reserved "endif"
     return $ If cond  stmt1 


ifElseStmt :: Parser (Stmt ())
ifElseStmt =
  do reserved "if"
     cond  <- expression 
     reserved "then"
     stmt1 <- many statement
     reserved "else"
     stmt2 <- many statement
     reserved "endif"
     return $ IfElse  cond  stmt1 stmt2 

whileStmt :: Parser (Stmt ())
whileStmt =
  do reserved "while"
     cond <- expression 
     reserved "do"
     stmt <- many statement
     reserved "done"
     return $ While cond stmt

assignStmt :: Parser (Stmt ()) 
assignStmt =
  do var  <- Val <$> identifier <*> pure () -- Val String a is the type we want.
     reservedOp "="                         --We fmap the val constructor over the identifier, and then apply the pure() to make it type check. 
     expr <- expression
     semi
     return $ Assn var  expr

--NEED TO DO
idStmt::Parser (Stmt ())
idStmt = do
    ident <- identifier
    return $  IdStmt ident

readStmt:: Parser (Stmt ()) --get line and return it
readStmt = do 
    reserved "read"
    str <- identifier 
    semi
    return $ Read (Val str ())

printStmt ::Parser (Stmt ())
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

expression :: Parser (Expr ())
expression = buildExpressionParser operators term

operators = [ [Prefix (reservedOp "-" >> return (Neg ()            ))          ]
             , [Infix  (reservedOp "*"   >> return (Binary () Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (Binary () Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (Binary () Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (Binary () Subtract)) AssocLeft]
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
        if (head num) == '0' && (length num) > 1 then fail "0 error"  else return $ read num

letters = ['0'..'9']++['a'..'z']++['A'..'Z']++[',','.','!','!',' ']

stringParser:: Parser String
stringParser = 
    do
        char '"'
        str <- many $ oneOf letters 
        char '"'
        return $ str

--generate c code
term::Parser (Expr ()) 
term = parens expression  
    <|>liftM2 Var (pure ()) identifier
    <|> try ( liftM2 FloatConst (pure ()) floatParser) 
    <|> try (liftM2 IntConst (pure ()) integerParser)
    <|> liftM2 StringEx (pure ()) stringParser
    -- <|> liftM FloatConst float --FloatConst Double 
    --get as a list of numbers, and just zip with increasing powers of 10 to actually make it a number


--Pretty Printer
class PrettyPrint a where
    prettyPrint::a->String

pretty::(PrettyPrint a)=>a->String
pretty a = prettyPrint a

instance PrettyPrint (Expr t) where
    prettyPrint (Neg _ a) = ("-" ++ (prettyPrint a)) 
    prettyPrint (Var _ a) =  a
    prettyPrint (IntConst _ a) = show a
    prettyPrint (FloatConst _ a) = show a
    prettyPrint (StringEx  _ a) = a
    prettyPrint (Binary _ op a b) =  read $ (prettyPrint a) ++ (prettyPrint op) ++ (prettyPrint b)

instance PrettyPrint (Stmt u) where
    prettyPrint (Seq a) = prettyPrint a
    prettyPrint (If a b) =  "if " ++ (prettyPrint a) ++ " then " ++ (prettyPrint b) 
    prettyPrint (IfElse a b c) = "if " ++ (prettyPrint a) ++ " then " ++ (prettyPrint b) ++ " else " ++ (prettyPrint c)
    prettyPrint (While a b) =  "while " ++ (prettyPrint a) ++ " do " ++ (prettyPrint b) ++ " done" 
    prettyPrint (Print a) = prettyPrint a ++ ";"
    prettyPrint (Read a) =  prettyPrint a ++ ";"
    prettyPrint (Assn a b) =  prettyPrint a ++ " = " ++  (prettyPrint b) ++ ";"
    prettyPrint (IdStmt a) = a

instance PrettyPrint (Id a) where
    prettyPrint (Val s  _) = s 


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

instance PrettyPrint (Program u) where
    prettyPrint (Program a b) = (prettyPrint a) ++ (prettyPrint b)

instance PrettyPrint AssocMap where
    prettyPrint (AssocMap (a, b)) = a ++ ":" ++ (prettyPrint b) ++ "\n" 



getDecorator::Type->String
getDecorator (FloatType) = "%f"
getDecorator (IntType) = "%d"
getDecorator (StringType) = "%s"

getTypeFromID::Id Type->Type
getTypeFromID (Val a b) = b

cHeaders =  "  #include <stdio.h>\n \ 
\ #include <stdlib.h>\n \
\ #include <string.h>\n \


\ char* concat(char *str1, char *str2)\n \
\ {\n \
        
\        char *result = malloc(strlen(str1)+strlen(str2));\n \
\        strcpy(result, str1);\n \
\        strcat(result, str2);\n \
\        return result;\n \
\ }\n \


\ void rev(char *str)\n \
\ {\n \
\    char *start = str;\n \
\    char *end = start + strlen(str) - 1;\n \
\    char temp;\n \
\    while (end > start)\n \
\    {\n \
\        temp = *start;\n \
\        *start = *end;\n \
\        *end = temp;\n \
\        ++start;\n \
\        --end;\n \
\    }\n \
\ }  "

toC::CPrint a => a->String
toC a = cPrint a

class CPrint a where
    cPrint::a->String

instance CPrint a => (CPrint [a]) where
    cPrint a = concat $ map cPrint a

instance CPrint Type  where
    cPrint (FloatType) = "float"
    cPrint (IntType) = "int"
    cPrint (StringType) = "char*" 

instance CPrint (Program Type) where
    cPrint (Program a b) = cHeaders ++  "int main() {\n" ++ (cPrint a)++ "\n" ++ (cPrint b) ++ "\n}\n"

instance CPrint Decl where
    cPrint (DecSeq a) = cPrint a
    cPrint (Dec a b) =  (cPrint b) ++ " "++ (a)  ++ ";\n"

instance CPrint (Expr Type) where
    cPrint (Neg t a) = 
        case t of
            (IntType) -> "-" ++ "(" ++ cPrint a ++ ")" 
            (FloatType) -> "-" ++ "(" ++ cPrint a ++ ")" 
            (StringType) ->"rev(" ++cPrint a ++" )"  
    cPrint (Var t a) =  a
    cPrint (IntConst t a) = show a
    cPrint (FloatConst t a) = show a
    cPrint (StringEx t a) = a
    cPrint (Binary t op a b) = 
        case t of 
            (StringType) ->
                case op of
                    (Add) -> "concat(" ++ (cPrint a) ++", " ++ (cPrint b) ++ ")"
                    (Subtract) -> "concat(" ++ (cPrint a) ++", rev(" ++  (cPrint b) ++"));"
            (FloatType) -> (cPrint a) ++ (cPrint op) ++ (cPrint b)
            (IntType) -> (cPrint a) ++ (cPrint op) ++ (cPrint b)

instance CPrint (Stmt Type) where
    cPrint (Seq a) = cPrint a
    cPrint (If a b) =  "if " ++ "(" ++ (cPrint a) ++ "){\n" ++  (cPrint b) ++ "}\n\n" 
    cPrint (IfElse a b c) = "if " ++ "(" ++ (cPrint a) ++ "){\n" ++  (cPrint b) ++  "}else{\n" ++ (cPrint c) ++ "}\n\n"  
    cPrint (While a b) =  "while " ++ "(" ++  (cPrint a) ++ "){\n" ++ (cPrint b) ++ "}\n\n" 
    cPrint (Print a) = "printf(\"" ++ (getDecorator $  getTypeFrom a) ++ "\",&"  ++ cPrint a ++ ");\n\n"
    cPrint (Read a) =  "scanf(\"" ++ (getDecorator $ (getTypeFromID a)) ++ "\", &" ++ cPrint a ++" );\n\n"
    cPrint (Assn a b) =  cPrint a ++ " = " ++  (cPrint b) ++ ";\n"
    cPrint (IdStmt a) =  a

instance CPrint (Id a) where
    cPrint (Val s  t) = s 

instance CPrint BinOp where
    cPrint Add = "+"
    cPrint Multiply = "*"
    cPrint Subtract = "-"
    cPrint Divide = "/"

typeCheckProgram::M.Map (Id ()) Type->Program ()->Either SemError (Program Type) 
typeCheckProgram m (Program decList stList) = 
    do
        typeCheckDecList decList 
        case typeCheckStmtList m stList of 
            Left e -> Left e
            Right r -> Right $ Program decList r

--checks to see if an entire list has any redeclarations
typeCheckDecList::[Decl]->Either SemError () 
typeCheckDecList dl = 
    do
        let l = map (\(Dec i t)-> i) dl 
        let x = filter ((>1) . length) $ group l
        if ((length x) <= 1) then Right () else Left ( ReDecs "Redeclaration Error")

typeCheckStmtList::M.Map (Id ()) Type->[Stmt ()]->Either SemError [Stmt Type] 
typeCheckStmtList m x = mapM (typeCheckStmt m) x 

--data  =  (Id, Type) |  (Expr, Type) |  (Stmt, Type)
typeCheckStmt::M.Map (Id ()) Type->Stmt ()->Either SemError (Stmt Type)
typeCheckStmt m (Assn a b) = case  M.lookup a m  of 
    Just t -> do 
        l <- (typeCheckExpr m b) 
        if t == getTypeFrom l --Remember, l is a type wrapped in an Id constructor.
            then Right (Assn (fmap (const t) a) l) 
                else Left $ CheckError "Error in Assignment Statement Types"  
    Nothing ->  Left $ CheckError "Error in Assignment Statement Types"  

--CHEK THAT THE THING I'M ASSIGNING TO IS THE THING I'M ASSIGNING OF
--check that I am assigning value of expression to an ID
--Expr type has to match id type FUCK FUCK FUCK
--PAtTERN MATCH

typeCheckStmt m (If a b) = do 
        l <- (typeCheckExpr m a) 
        if getTypeFrom l == IntType 
            then do 
                t <- (typeCheckStmtList m b)
                Right  $ (If l t)
            else Left $ CheckError "Error in If Statement Types"


typeCheckStmt m (IfElse a b c) = do
    l <- (typeCheckExpr m a) 
    if getTypeFrom l == IntType 
        then do 
            t <- (typeCheckStmtList m b)
            q <- (typeCheckStmtList m c)
            Right   (IfElse l t q)
        else Left $ CheckError "Error in IfElse Statement Types"


typeCheckStmt m (While a b) = do
        l <- (typeCheckExpr m a)
        if getTypeFrom l ==IntType 
            then do
                t <- (typeCheckStmtList m b)
                Right $ (While l t ) 
            else Left $ CheckError "Error in While Statement Types"

typeCheckStmt m (Read a) = do 
    case M.lookup a m of 
        Just t -> Right $ Read $ fmap (const t) a
        Nothing -> Left $ CheckError "Error in Read"

typeCheckStmt m (Print a) = do
    l <- (typeCheckExpr m a)
    Right $ (Print l)  

getTypeFrom::(Expr Type)->Type
--getTypeFrom = undefined
getTypeFrom (Var a _) = a
getTypeFrom (IntConst a _) = a
getTypeFrom (FloatConst a _) = a
getTypeFrom (StringEx a _) = a
getTypeFrom (Neg _ a) = getTypeFrom a
getTypeFrom (Binary a _ _ _) = a

typeCheckExpr::M.Map (Id ()) Type->Expr ()->Either SemError (Expr Type)
typeCheckExpr m (Var _ a) =  do 
    case M.lookup (Val a ()) m of 
        Just t -> Right $ Var t a
        Nothing -> Left $ CheckError "Error in Var"

typeCheckExpr m (IntConst _ a) = Right $ IntConst IntType a
typeCheckExpr m (FloatConst _ a) = Right $ FloatConst FloatType a
typeCheckExpr m (Binary _ op a b) =  typeCheckBinOp m op a b 
typeCheckExpr m (Neg _ a) = do
    l <- (typeCheckExpr m a) 
    let r =  getTypeFrom l
    Right $ Neg r l  

typeCheckExpr m (StringEx  _ a) = Right $ StringEx StringType a

typeCheckBinOp::M.Map (Id ()) Type ->BinOp->Expr ()->Expr ()->Either SemError (Expr Type) 
typeCheckBinOp m op l r = 
    do 
        lt <- typeCheckExpr m l
        rt <- typeCheckExpr m r
        let lperm = getTypeLOp op
        let rperm = getTypeROp op $ getTypeFrom lt
        if elem (getTypeFrom lt) lperm && elem (getTypeFrom rt) rperm then 
            case ((getTypeFrom lt), (getTypeFrom rt)) of  
                (FloatType, _) -> Right (Binary FloatType op lt rt )
                (_, FloatType) -> Right (Binary FloatType op lt rt )
                (IntType, IntType) -> Right (Binary IntType op lt rt )
                (StringType, StringType) -> Right (Binary StringType op lt rt )
            else Left $ CheckError "Type Mismatch in Binary Op"   

--The first, given a binary operator as input, produces the list of all the types that can go in the left operand.
getTypeLOp::BinOp->[Type]
getTypeLOp (Add) = [IntType, FloatType, StringType]
getTypeLOp (Multiply) = [IntType, FloatType]
getTypeLOp (Divide) = [IntType, FloatType] 
getTypeLOp (Subtract) = [IntType, FloatType, StringType]

--The second, given a binary operator and a type, outputs the list of all the types that can go in the right operand.
getTypeROp::BinOp->Type->[Type]
getTypeROp (Add) IntType = [IntType, FloatType]
getTypeROp (Subtract) IntType = [IntType, FloatType]
getTypeROp (Divide) IntType = [IntType, FloatType]
getTypeROp (Multiply) IntType = [IntType, FloatType]
getTypeROp (Add) FloatType= [IntType, FloatType]
getTypeROp (Subtract) FloatType = [IntType, FloatType]
getTypeROp (Divide) FloatType = [IntType, FloatType]
getTypeROp (Multiply) FloatType = [IntType, FloatType]
getTypeROp (Add) StringType= [StringType] 
getTypeROp (Subtract) StringType = [StringType]

--Want to make sure types match up.
--typeCheckStmt m (IdStmt a) = --if the type of the id and the stmt are the same 

parseString :: String -> (Program ())
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

parseFileAndCheck :: String -> IO (Program Type) 
parseFileAndCheck file =
  do program  <- readFile file
     case parse (miniParser <* eof) file program   of
       Left e -> fail "ERROR PARSING" 
       Right r -> typeCheck r file

cPrettyPrinter::String->(Program Type)->IO ()
cPrettyPrinter fileName a = do    
        handle <- openFile (fileName -<.> ".c") WriteMode            
        hPutStr handle (cPrint a)
        hClose handle


--data Program a = Program [Decl] [Stmt a] deriving(Eq,Show)
symAdd::Decl->AssocMap
symAdd (Dec a b) = AssocMap(a,b)

typeCheck::Program ()->String->IO (Program Type)
typeCheck (Program a b) fileName = do
        handle <- openFile (replaceExtension fileName ".symbol.txt") WriteMode
        let assocMapList = map symAdd a --list of map 
        let m = map pretty assocMapList
        let am = M.fromList $ map (\(AssocMap(c,d))->(Val c (), d)) assocMapList 
        let c = typeCheckProgram am (Program a b)
        --writeFile fileName $ concat m
        hPutStr handle $ concat m 
        hClose handle
        case c of
            Right c -> (cPrettyPrinter fileName c) >>return c --feed C into the cTypeChecker
            Left e-> case e of 
                (CheckError e) -> putStrLn  e >> exitFailure --TODO: EXIT ON FAILURE 
                (IncompatibleTypes e) -> putStrLn  e >> exitFailure --TODO: EXIT ON FAILURE 
                (NoDecs e) -> putStrLn  e >> exitFailure --TODO: EXIT ON FAILURE 
                (ReDecs e) ->  putStrLn  e >> exitFailure --TODO: EXIT ON FAILURE 

main = do 
    (arg:_) <- getArgs 
    parseFile arg


--TODO:
--pretty print for files
--cprint for files
--
--string reverse

