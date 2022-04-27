{-#LANGUAGE TupleSections #-}
module TypeInfer where

import Control.Monad.Trans.State
import Control.Monad

import Text.Parsec.Pos

import Ast



known :: Typ -> Bool
known (Unknown _) = False
known (UnknownFun _ _) = False
known Nat = True
known Char = True

known (List a) = known a
known (Arrow as a) = and (map known as) && known a


-- 

data TypeCheckError 
    = TypeMismatch Typ Typ String
    | TypeType Typ String
    | Unassigned Symbol
    | ArgCount Symbol Int Int
    | Unresolved Symbol Typ

instance Show TypeCheckError where
    show (TypeMismatch t1 t2 msg) = 
        "Cannot match <" ++ show t1 ++ "> with <" ++ show t2 ++ ">: " ++ msg
    show (Unassigned sym ) = "Symbol \""++ sym ++ "\" is undefined"
    show (TypeType typ msg) = "Unexpected type <" ++ show typ ++">: " ++msg
    show (ArgCount sym n1 n2) = 
        "Wrong number of arguments to \""++sym++"\": expected "++show n1++", given "++show n2
    show (Unresolved sym t) =
        "Unresolved symbol \"" ++ sym ++ "\" of type <" ++ show t ++">"


data TypeCheckContextEntry  = Def {defSym :: Symbol, defTyp :: Typ}
                            | FrameSep

instance Show TypeCheckContextEntry where
    show (Def sym typ) = sym ++ " : " ++ show typ
    show FrameSep = "-separator-"

contextEntryUnknown FrameSep = False
contextEntryUnknown (Def _ typ) = not $ known typ

data TypeCheckContext = TypeCheckContext {
        decl :: [TypeCheckContextEntry],
        varIndex :: Int,
        pos :: SourcePos
    }

type TypeCheckEnvironment = StateT TypeCheckContext (Either TypeCheckError)

---
--StateT 'lenses'

getDecl = decl <$> get
putDecl dec = do 
    ctx <- get
    put $ ctx {decl = dec}
modifyDecl f = do
    ctx <- get
    put $ ctx {decl = f $ decl ctx}
getsDecl = (<$>getDecl)


getIndex = do 
    ctx <- get
    let num = varIndex ctx
    put $ ctx {varIndex = num + 1}
    return num

setPos pos = do 
    ctx <- get
    put $ ctx {pos = pos}

getPos = gets pos
---

instantiateTypeVariable :: Int -> Typ -> [TypeCheckContextEntry] -> [TypeCheckContextEntry]
instantiateTypeVariable var val = map go 
    where 
        go FrameSep = FrameSep
        go (Def ctx typ) = Def ctx $ fix typ
        
        fix Nat = Nat
        fix Char = Char
        fix (List a) = List $ fix a
        fix (Arrow args ret) = Arrow (map fix args) $ fix ret
        fix (Unknown n) 
            | n == var = val
            | otherwise = Unknown n
        
        fix (UnknownFun n res)
            | n == var = val
            | otherwise = UnknownFun n $ fix res

matchTyp :: Typ -> Typ -> TypeCheckEnvironment Typ
matchTyp (Unknown id) b = do
    modifyDecl $ instantiateTypeVariable id b
    return b
matchTyp a (Unknown id) = do
    modifyDecl $ instantiateTypeVariable id a
    return a
matchTyp Nat Nat = return Nat
matchTyp Char Char = return Char
matchTyp (List a) (List b) = do
    sub <- matchTyp a b
    return $ List sub

matchTyp (Arrow args1 ret1) (Arrow args2 ret2) = do 
    ret <- matchTyp ret1 ret2
    args <- mapM (uncurry matchTyp) $ zip args1 args2
    return $ Arrow args ret

matchTyp (UnknownFun ida a) (UnknownFun idb b) = do
    resret <- matchTyp a b
    nextId <- getIndex
    let newVar = UnknownFun nextId resret
    modifyDecl $ (
        instantiateTypeVariable ida newVar
        . instantiateTypeVariable idb newVar)
    return $ UnknownFun nextId resret

matchTyp (UnknownFun ida a) (Arrow bs b) = do
    res <- matchTyp a b
    let newType = Arrow bs res
    modifyDecl $ instantiateTypeVariable ida newType
    return newType

matchTyp (Arrow as a) (UnknownFun idb b) = do
    res <- matchTyp a b
    let newType = Arrow as res
    modifyDecl $ instantiateTypeVariable idb newType
    return newType

matchTyp a b = do
    pos <- getPos
    failTC $ TypeMismatch a b $ "at " ++ show pos

---

failTC :: TypeCheckError -> TypeCheckEnvironment a
failTC = StateT . const . Left 

lookupMatchOrAdd :: Symbol -> Typ -> TypeCheckEnvironment Typ
lookupMatchOrAdd sym typ = do
    ctx <- getDecl
    go ctx
    where
        go :: [TypeCheckContextEntry] -> TypeCheckEnvironment Typ
        go [] = const typ <$> modifyDecl (Def sym typ :)
        go (FrameSep:xs) = go xs
        go (Def s t:xs)
            | s == sym = matchTyp t typ
            | otherwise = go xs

        

---

typeCheckExprMeta :: Typ -> PExpression -> TypeCheckEnvironment Typ
typeCheckExprMeta typ mexpr = do
    setPos $ meta mexpr
    typ <- typeCheckExpr typ $ value mexpr
    return typ

typeCheckExpr :: Typ -> Expression SourcePos  -> TypeCheckEnvironment Typ
typeCheckExpr typ (Variable s) = lookupMatchOrAdd s typ

typeCheckExpr typ (IntLit _) =
    matchTyp typ Nat

typeCheckExpr typ (CharLit _) =
    matchTyp typ Char

typeCheckExpr typ (Function name args) = do
    index <- getIndex
    funtyp <- lookupMatchOrAdd name (UnknownFun index typ)
    argTypes <-
        case funtyp of 
            UnknownFun _ _ -> forM args $ \arg -> do
                idx <- getIndex
                typeCheckExprMeta (Unknown idx) arg
            Arrow expectedArgs _ -> do
                let 
                    nexp = length expectedArgs
                    ngiv = length args
                when (nexp /= ngiv) $ 
                    failTC $ ArgCount name nexp ngiv
                zipWithM typeCheckExprMeta expectedArgs args
            _ -> failTC $ TypeType funtyp "Not a function type"
    idx <- getIndex
    res <- lookupMatchOrAdd name $ Arrow argTypes $  Unknown idx
    case res of
        UnknownFun _ a -> return a
        Arrow _ a -> return a
        _ -> failTC $ TypeType res "Not a function type 2"

---

typeCheckStatements :: [PStatement] -> TypeCheckEnvironment ()
typeCheckStatements stmnts = const () <$> mapM typeCheckStatementMeta stmnts

typeCheckStatementMeta :: PStatement -> TypeCheckEnvironment ()
typeCheckStatementMeta mstmt = do
    setPos $ meta mstmt
    typeCheckStatement $ value mstmt

typeCheckStatement :: (Statement SourcePos SourcePos) -> TypeCheckEnvironment () 
typeCheckStatement (Assignment sym expr) = do
    idx <- getIndex
    typ <- lookupMatchOrAdd sym $ Unknown idx
    typ <- typeCheckExprMeta typ expr
    lookupMatchOrAdd sym typ
    return ()

typeCheckStatement (Push sym expr) = do
    idx <- getIndex
    lsttyp <- lookupMatchOrAdd sym $ List $ Unknown idx
    let typ = case lsttyp of List a -> a
    typ <- typeCheckExprMeta typ expr
    lookupMatchOrAdd sym (List typ)
    return ()

typeCheckStatement (Pop sym expr) = do
    idx <- getIndex
    lsttyp <- lookupMatchOrAdd sym $ List $ Unknown idx
    let typ = case lsttyp of List a -> a
    typ <- typeCheckExprMeta typ expr
    lookupMatchOrAdd sym (List typ)
    return ()

typeCheckStatement (Expr expr) = do
    idx <- getIndex
    typeCheckExprMeta (Unknown idx) expr
    return ()

typeCheckStatement (If expr stmnts1 stmnts2) = do
    typeCheckExprMeta Nat expr
    typeCheckStatements stmnts1
    typeCheckStatements stmnts2

typeCheckStatement (While expr stmts) = do 
    typeCheckExprMeta Nat expr
    typeCheckStatements stmts

---

renumberUnknowns :: Typ -> TypeCheckEnvironment Typ
renumberUnknowns (Unknown _) = do
    idx <- getIndex
    return $ Unknown idx
renumberUnknowns (UnknownFun _ a) = do
    a' <- renumberUnknowns a
    idx <- getIndex
    return $ UnknownFun idx a'
renumberUnknowns Nat = return Nat
renumberUnknowns Char = return Char
renumberUnknowns (List a)= List <$> renumberUnknowns a 
renumberUnknowns (Arrow as a) = 
    Arrow <$> mapM renumberUnknowns as <*> renumberUnknowns a


---

typeCheckFunDefs :: PProgram -> TypeCheckEnvironment ()
typeCheckFunDefs defs = const () <$> mapM typeCheckFunDefMeta defs

typeCheckFunDefMeta :: PFunDef -> TypeCheckEnvironment ()
typeCheckFunDefMeta fd = do
    setPos $ meta fd
    typeCheckFunDef $ value fd

typeCheckFunDef :: (FunDef SourcePos SourcePos SourcePos) -> TypeCheckEnvironment ()
typeCheckFunDef (FunDef name args body returnValue) = do
    modifyDecl pushFrame
    args <- mapM (\(s, t) -> (s,) <$> renumberUnknowns t) args
    mapM (uncurry lookupMatchOrAdd) args
    typeCheckStatements body
    idx <- getIndex
    retType <- typeCheckExprMeta (Unknown idx) returnValue
    argTypes <- mapM (uncurry lookupMatchOrAdd) args
    ctx <- getDecl
    case filter contextEntryUnknown ctx of
        [] -> return ()
        Def sym typ:_ -> failTC $ Unresolved sym typ
        _ -> undefined
    putDecl $ popFrame ctx
    lookupMatchOrAdd name (Arrow argTypes retType)
    return ()
    where 
        popFrame (FrameSep:xs) = xs
        popFrame (x:xs) = popFrame xs
        popFrame [] = []

        pushFrame xs = FrameSep:xs

---
initialContext = TypeCheckContext{
    decl =  [
        Def "in" $ Arrow [] Char, 
        Def "out" $ Arrow [Char] Nat, -- no void type yet
        Def "inc" $ Arrow [Nat] Nat,
        Def "dec" $ Arrow [Nat] Nat,
        Def "atoi" $ Arrow [Char] Nat,
        Def "itoa" $ Arrow [Nat] Char
        ],
    varIndex = 1,
    pos = newPos "<undefined>" 0 0
}

typeCheck :: PProgram -> Either TypeCheckError [TypeCheckContextEntry]
typeCheck prog = 
    case execStateT (typeCheckFunDefs prog) initialContext  of
        Left err -> Left err
        Right ctx -> case filter contextEntryUnknown (decl ctx) of
            [] -> Right $ decl ctx
            Def n t:xs -> Left $ Unresolved n t
            FrameSep:xs -> undefined --should be impossible 
        