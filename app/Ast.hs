module Ast where

import Text.Parsec
import Data.List

data CompileError = ParseError ParseError | GenError String 
                    deriving Show

type Symbol = String

data Typ = Unknown Int
         | Char
         | Nat
         | List Typ 
         | Arrow [Typ] Typ 
         | UnknownFun Int Typ
    deriving (Eq)

instance Show Typ where
    show (Unknown n) = "?"++show n
    show Char = "char"
    show Nat = "nat"
    show (List a) = "list("++show a++")"
    show (Arrow args res) = 
        "function(" ++ 
        intercalate ", " (map show args)
        ++") -> "++show res
    show (UnknownFun n res) = 
        "function ?"++show n++" -> "++ show res

data Expression = Variable Symbol
                | IntLit Int
                | CharLit Char
                | Function Symbol [Expression]
                deriving Show

data Statement  = Assignment Symbol Expression
                | Push Symbol Expression
                | Pop Symbol Expression
                | Expr Expression
                | If Expression [Statement] [Statement]
                | While Expression [Statement] 
                deriving Show

data FunDef = FunDef {
        name :: Symbol,
        args :: [(Symbol, Typ)],
        body :: [Statement],
        returnValue :: Expression
    } 
    deriving Show

type Program = [FunDef]