module Types where

data Program =  Program DeclarationsList StatementList 
  deriving Show
  
  
--The Makes Sense line. 

data DeclarationsList = DeclarationsList DeclarationsList Declaration 
                    | DEmpty
    deriving Show

data Declaration = DeclarationInt String Int  
                |DeclarationFloat String Float
                |DeclarationString String String
                deriving Show


data StatementList = StatementList StatementList Statement
                    | SEmpty
                    deriving Show

data Statement = IfState String StatementList
                |IfElseState String StatementList StatementList
                |WhileState String StatementList
                |PrintState Line
                |ReadState Line
                |AssnStatement String Exp
                deriving Show

data IntLit = Int
    deriving Show

data FloatLit = Float
    deriving Show

data Id = Id 
    deriving Show

data NUM = NumInt Int 
        |NumFloat Float
        deriving Show
           
data Line = StringLit String
            |StringExp Exp 
            deriving Show

data Exp = Mult Exp Exp
        |Plus Exp Exp
        |Subt Exp Exp
        |Divi Exp Exp
        |Neg Exp
        |NUM NUM
        deriving Show


{-
data Tokens = TokenPlus
            |TokenMult
            |TokenMinus
            |TokenDiv
            |TokenEq
            |TokenQuote
            |TokenInt
            |TokenFloat
            |TokenString
            |TokenIf
            |TokenThen
            |TokenElse
            |TokenEndif
            |TokenWhile
            |TokenDone
            |TokenDo
            |TokenRead
            |TokenPrint
            |TokenId
            |TokenVar
            |TokenSemicolon
            |Tokencolon
            deriving Show

-}
--The lists need more nuance
--
