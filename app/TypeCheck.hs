module TypeCheck where

import Prelude hiding (lookup)
import Control.Monad.Trans.State
import Control.Monad
import Ast

---

data TypeContextEntry 
    = Def Symbol Typ
    | FrameSep

type TypeContext = [TypeContextEntry]
---

data TypeCheckError 
    = TypeMismatch Typ Typ String
    | TypeType Typ String
    | Unassigned Symbol
    | ArgCount Symbol Int Int

instance Show TypeCheckError where
    show (TypeMismatch t1 t2 msg) = 
        "Cannot match " ++ show t1 ++ " " ++ show t2 ++ ": " ++ msg
    show (Unassigned sym ) = "Symbol "++ sym ++ " is undefined"
    show (TypeType typ msg) = "Unexpected type " ++ show typ ++": " ++msg
    show (ArgCount sym n1 n2) = 
        "Wrong number of arguments to "++sym++": expected "++show n1++", given "++show n2

---

type TypeCheckEnvironment = StateT TypeContext (Either TypeCheckError)

---

failTC :: TypeCheckError -> TypeCheckEnvironment a
failTC = StateT . const . Left

lookup' :: Symbol -> TypeContext -> Maybe Typ
lookup' s [] = Nothing
lookup' s (Def sym typ: rest) 
    | s == sym = Just typ
    | otherwise = lookup' s rest
lookup' s (_:rest) = lookup' s rest

lookup :: Symbol -> TypeCheckEnvironment Typ
lookup s = do
    ctx <- get
    case lookup' s ctx of
        Just typ -> return typ
        Nothing -> failTC $ Unassigned s

lookupMatchOrAdd :: Symbol -> Typ -> TypeCheckEnvironment ()
lookupMatchOrAdd s t = do
    typ <- gets (lookup' s)
    case typ of
        Nothing -> modify (Def s t :)
        Just symTyp -> when (symTyp /= t) $ failTC $ TypeMismatch symTyp t $ "Symbol " ++ s

---

typeCheckExpr :: Expression -> TypeCheckEnvironment Typ

typeCheckExpr (IntLit _) = return Byte
typeCheckExpr (CharLit _) = return Byte

typeCheckExpr (Variable sym) = lookup sym

typeCheckExpr (Function name args) = do
    funType <- lookup name
    (expectedArgTypes, retType) <- 
        case funType of 
            (Arrow args ret) -> return (args, ret)
            _ -> failTC $ TypeType funType "Expected function"
    let 
        nExpectedArgs = length expectedArgTypes
        nGivenArgs = length args
    when (nExpectedArgs /= nGivenArgs) $ failTC $ ArgCount name nExpectedArgs nGivenArgs
    mapM matchArgTypes $ zip expectedArgTypes args
    return retType
    where
        matchArgTypes (expectedType, expr) = do
            givenType <- typeCheckExpr expr
            when (expectedType /= givenType) $ failTC $ 
                TypeMismatch expectedType givenType $ "argument for " ++ name

--- 

typeCheckStatements :: [Statement] -> TypeCheckEnvironment ()
typeCheckStatements stmnts = mapM typeCheckStatement stmnts >> return ()

typeCheckStatement :: Statement -> TypeCheckEnvironment ()
typeCheckStatement (Assignment sym expr) = do
    exprType <- typeCheckExpr expr
    lookupMatchOrAdd sym exprType

typeCheckStatement (Push sym expr) = do
    exprType <- typeCheckExpr expr
    lookupMatchOrAdd sym (List exprType)

typeCheckStatement (Pop sym expr) = do
    exprType <- typeCheckExpr expr
    lookupMatchOrAdd sym (List exprType)

typeCheckStatement (Expr expr) = const () <$> typeCheckExpr expr

typeCheckStatement (If expr stmnts1 stmnts2) = do
    cond <- typeCheckExpr expr
    when (cond /= Byte ) $ failTC $ TypeType cond "must be byte for if statement"
    typeCheckStatements stmnts1
    typeCheckStatements stmnts2

typeCheckStatement (While expr stmnts) = do
    cond <- typeCheckExpr expr
    when (cond /= Byte) $ failTC $ TypeType cond "must be byte for while statement"
    typeCheckStatements stmnts

---

typeCheckFunction :: FunDef -> TypeCheckEnvironment ()
typeCheckFunction (FunDef name args body ret) = do 
    argTypes <- forM args (\(sym, typ) -> do
        lookupMatchOrAdd sym typ
        return typ
        )
    typeCheckStatements body
    retType <- typeCheckExpr ret
    lookupMatchOrAdd name $ Arrow argTypes retType

typeCheckProgram :: Program -> TypeCheckEnvironment ()
typeCheckProgram program = const () <$> mapM typeCheckFunction program 

---

initialContext :: TypeContext
initialContext = [
    Def "in" $ Arrow [] Byte, 
    Def "out" $ Arrow [Byte] Byte, -- no void type yet
    Def "inc" $ Arrow [Byte] Byte,
    Def "dec" $ Arrow [Byte] Byte
    ]

typeCheck :: Program -> Maybe TypeCheckError
typeCheck program = 
    case evalStateT (typeCheckProgram program) initialContext of
        Right () -> Nothing
        Left err -> Just err