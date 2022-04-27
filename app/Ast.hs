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

data Meta m d = Meta {
        meta :: m,
        value :: d m
    }deriving Show

data Expression m   = Variable Symbol
                    | IntLit Int
                    | CharLit Char
                    | Function Symbol [Meta m Expression]
                    deriving Show

data Statement e m  = Assignment Symbol (Meta e Expression)
                    | Push Symbol (Meta e Expression)
                    | Pop Symbol (Meta e Expression)
                    | Expr (Meta e Expression)
                    | If (Meta e Expression) [Meta m (Statement e)] [Meta m (Statement e)]
                    | While (Meta e Expression) [Meta m (Statement e)] 
                    deriving Show

data FunDef e s f = FunDef {
        name :: Symbol,
        args :: [(Symbol, Typ)],
        body :: [Meta s (Statement e)],
        returnValue :: Meta e Expression
    } 
    deriving Show

type Program e s f = [Meta f (FunDef e s)]

type PProgram = Program SourcePos SourcePos SourcePos
type PFunDef = Meta SourcePos (FunDef SourcePos SourcePos)
type PStatement = Meta SourcePos (Statement SourcePos)
type PExpression = Meta SourcePos (Expression)