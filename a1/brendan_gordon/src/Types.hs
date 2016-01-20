module Types where

data Program =  Program DeclarationsList StatementList 
  deriving Show
  
  
--The Makes Sense line. 

data DeclarationsList = DeclarationsList DeclarationsList Declaration 
                    | Empty
    deriving Show

data Declarations = DeclarationInt Id IntLit  
                |DeclarationFloat Id FloatLit
                |DeclarationString Id String
                deriving Show


data StatementList = StatementList StatementList Statement
                    | Empty
                    deriving Show

data Statement = IfState Id StatementList
                |IfElseState Id StatementList StatementList
                |WhileState Id StatementList
                |PrintState Line
                |ReadState Line
                |AssnStatement Id Exp
                deriving Show

data Num = NumInt Int 
        |NumFloat Float
        deriving Show
           
data Line = StringLit String
            |StringExp Exp 

data Exp = Mult Exp Exp
        |Plus Exp Exp
        |Subt Exp Exp
        |Divi Exp Exp
        |Neg Exp
        |Num Num
        deriving Show


--The lists need more nuance
--
