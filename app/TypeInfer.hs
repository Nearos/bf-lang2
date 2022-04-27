{-#LANGUAGE TupleSections #-}
module TypeInfer where

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad
import Data.Maybe

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
        pos :: SourcePos,
        refTypes :: [(Int, Typ)],
        refTypeIndex :: Int
    }

type TypeCheckEnvironment = StateT TypeCheckContext (Either TypeCheckError)
---
-- Output Types

type FunDefMeta = [(Symbol, Int)]

type IProgram = Program Int () FunDefMeta
type IFunDef = Meta FunDefMeta (FunDef Int ())
type IStatement = Meta () (Statement Int)
type IExpression = Meta Int Expression

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

modifyRef f = do
    ctx <- get
    put $ ctx {refTypes = f $ refTypes ctx}

addRef t = do
    ctx <- get
    let ret = refTypeIndex ctx 
    put $ ctx {
        refTypes = (ret, t) : refTypes ctx ,
        refTypeIndex = ret + 1
    }
    return ret

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

instantiateTypeVariable :: Int -> Typ -> TypeCheckEnvironment ()
instantiateTypeVariable var val = do
    modifyDecl (map go1)
    modifyRef (map go2)
    where 
        go1 FrameSep = FrameSep
        go1 (Def ctx typ) = Def ctx $ fix typ

        go2 (n, t) = (n, fix t)
        
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
    instantiateTypeVariable id b
    return b
matchTyp a (Unknown id) = do
    instantiateTypeVariable id a
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
    instantiateTypeVariable ida newVar
    instantiateTypeVariable idb newVar
    return $ UnknownFun nextId resret

matchTyp (UnknownFun ida a) (Arrow bs b) = do
    res <- matchTyp a b
    let newType = Arrow bs res
    instantiateTypeVariable ida newType
    return newType

matchTyp (Arrow as a) (UnknownFun idb b) = do
    res <- matchTyp a b
    let newType = Arrow as res
    instantiateTypeVariable idb newType
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

typeCheckExprMeta :: Typ -> PExpression -> TypeCheckEnvironment (IExpression, Typ)
typeCheckExprMeta typ mexpr = do
    setPos $ meta mexpr
    res <- typeCheckExpr typ $ value mexpr
    return res

typeCheckExpr :: Typ -> Expression SourcePos  -> TypeCheckEnvironment (IExpression, Typ)
typeCheckExpr typ (Variable s) = do
    typ <- lookupMatchOrAdd s typ
    idx <- addRef typ
    return (Meta idx $ Variable s, typ)

typeCheckExpr typ (IntLit a) = do
    typ <- matchTyp typ Nat
    idx <- addRef typ
    return (Meta idx $ IntLit a, typ)

typeCheckExpr typ (CharLit a) = do
    matchTyp typ Char
    idx <- addRef typ
    return (Meta idx $ CharLit a, typ)

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
    res <- lookupMatchOrAdd name 
            $ Arrow (map snd argTypes) 
            $  Unknown idx
    rtyp <- case res of
        UnknownFun _ a -> return a
        Arrow _ a -> return a
        _ -> failTC $ TypeType res "Not a function type 2"
    tvidx <- addRef rtyp
    return (Meta tvidx $ Function name $ map fst argTypes, 
            rtyp)

---

typeCheckStatements :: [PStatement] -> TypeCheckEnvironment [IStatement]
typeCheckStatements stmnts = mapM typeCheckStatementMeta stmnts

typeCheckStatementMeta :: PStatement -> TypeCheckEnvironment IStatement
typeCheckStatementMeta mstmt = do
    setPos $ meta mstmt
    typeCheckStatement $ value mstmt

typeCheckStatement :: (Statement SourcePos SourcePos) -> 
                        TypeCheckEnvironment IStatement
typeCheckStatement (Assignment sym expr) = do
    idx <- getIndex
    typ <- lookupMatchOrAdd sym $ Unknown idx
    typ <- typeCheckExprMeta typ expr
    lookupMatchOrAdd sym $ snd typ
    return $ Meta () $ Assignment sym $ fst typ

typeCheckStatement (Push sym expr) = do
    idx <- getIndex
    lsttyp <- lookupMatchOrAdd sym $ List $ Unknown idx
    let typ = case lsttyp of List a -> a
    typ <- typeCheckExprMeta typ expr
    lookupMatchOrAdd sym (List $ snd typ)
    return $ Meta () $ Push sym $ fst typ

typeCheckStatement (Pop sym expr) = do
    idx <- getIndex
    lsttyp <- lookupMatchOrAdd sym $ List $ Unknown idx
    let typ = case lsttyp of List a -> a
    typ <- typeCheckExprMeta typ expr
    lookupMatchOrAdd sym (List $ snd typ)
    return $ Meta () $ Pop sym $ fst typ

typeCheckStatement (Expr expr) = do
    idx <- getIndex
    typ <- typeCheckExprMeta (Unknown idx) expr
    return $ Meta () $ Expr $ fst typ

typeCheckStatement (If expr stmnts1 stmnts2) = do
    rcond <- typeCheckExprMeta Nat expr
    rstmnts1 <- typeCheckStatements stmnts1
    rstmnts2 <- typeCheckStatements stmnts2
    return $ Meta () $ If (fst rcond) rstmnts1 rstmnts2

typeCheckStatement (While expr stmts) = do 
    rcond <- typeCheckExprMeta Nat expr
    rstmnts <- typeCheckStatements stmts
    return $ Meta () $ While (fst rcond) rstmnts

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

typeCheckFunDefs :: PProgram -> TypeCheckEnvironment IProgram
typeCheckFunDefs defs = mapM typeCheckFunDefMeta defs

typeCheckFunDefMeta :: PFunDef -> TypeCheckEnvironment IFunDef
typeCheckFunDefMeta fd = do
    setPos $ meta fd
    typeCheckFunDef $ value fd

typeCheckFunDef :: (FunDef SourcePos SourcePos SourcePos) -> 
                    TypeCheckEnvironment IFunDef
typeCheckFunDef (FunDef name args body returnValue) = do
    modifyDecl pushFrame
    args <- mapM (\(s, t) -> (s,) <$> renumberUnknowns t) args
    mapM (uncurry lookupMatchOrAdd) args
    body' <- typeCheckStatements body
    idx <- getIndex
    retType <- typeCheckExprMeta (Unknown idx) returnValue
    argTypes <- mapM (uncurry lookupMatchOrAdd) args
    ctx <- getDecl
    case filter contextEntryUnknown ctx of
        [] -> return ()
        Def sym typ:_ -> failTC $ Unresolved sym typ
        _ -> undefined
    md <- addFrameTypeReferences ctx []
    lookupMatchOrAdd name (Arrow argTypes $ snd retType)
    return $ Meta md $ FunDef name args body' $ fst retType
    where 
        popFrame (FrameSep:xs) = xs
        popFrame (x:xs) = popFrame xs
        popFrame [] = []

        pushFrame xs = FrameSep:xs

        addFrameTypeReferences [] res = do
            putDecl []
            return res
        addFrameTypeReferences (FrameSep:rst) res = do
            putDecl rst
            return res
        addFrameTypeReferences (Def name typ:rst) res = do
            ref <- addRef typ
            addFrameTypeReferences rst $ (name, ref):res


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
    pos = newPos "<undefined>" 0 0,
    refTypes = [],
    refTypeIndex = 0
}

--- Post processing - resolving type references

type ResolveEnvironment = Reader [(Int, Typ)]

resolve :: Int -> ResolveEnvironment Typ
resolve n = asks $ fromJust . lookup n

type RFunDefMeta = [(Symbol, Typ)]

type RProgram = Program Typ () RFunDefMeta
type RFunDef = Meta RFunDefMeta (FunDef Typ ())
type RStatement = Meta () (Statement Typ)
type RExpression = Meta Typ Expression

resolveProgram :: IProgram -> ResolveEnvironment RProgram
resolveProgram ip = mapM resolveFunDefMeta ip 

resolveFunDefMeta :: IFunDef -> ResolveEnvironment RFunDef
resolveFunDefMeta (Meta meta fd) = do
    meta' <- mapM (\(name, val) -> (name, ) <$> resolve val) meta
    fd' <- resolveFunDef meta' fd
    return $ Meta meta' fd'

resolveFunDef meta' (FunDef name args body returnValue) = do
    let args' = map (\(name, _) -> (name, fromJust $ lookup name meta')) args
    FunDef name args' <$> resolveStatements body 
                        <*> resolveExprMeta returnValue

resolveStatements = mapM resolveStatementMeta

resolveStatementMeta (Meta () stmt) = Meta () <$> resolveStatement stmt

resolveStatement (Assignment s e) = Assignment s <$> resolveExprMeta e
resolveStatement (Push s e) = Push s <$> resolveExprMeta e 
resolveStatement (Pop s e) = Pop s <$> resolveExprMeta e 
resolveStatement (Expr e) = Expr <$> resolveExprMeta e 
resolveStatement (If e s1 s2) = If <$> resolveExprMeta e 
                                    <*> resolveStatements s1 
                                    <*> resolveStatements s2 
resolveStatement (While e s) = While    <$> resolveExprMeta e 
                                        <*> resolveStatements s

resolveExprMeta :: IExpression -> ResolveEnvironment RExpression
resolveExprMeta (Meta meta expr) = Meta <$> resolve meta <*> resolveExpr expr 

resolveExpr :: Expression Int -> ResolveEnvironment (Expression Typ)
resolveExpr (Function sym subexprs) = 
    Function sym <$> mapM resolveExprMeta subexprs

resolveExpr (Variable a) = return $ Variable a 
resolveExpr (IntLit a) = return $ IntLit a 
resolveExpr (CharLit a) = return $ CharLit a 

---

typeCheck :: PProgram -> Either TypeCheckError RProgram
typeCheck prog = 
    case runStateT (typeCheckFunDefs prog) initialContext  of
        Left err -> Left err
        Right (prog, ctx) -> case filter contextEntryUnknown (decl ctx) of
            Def n t:xs -> Left $ Unresolved n t
            FrameSep:xs -> undefined --should be impossible 
            [] -> Right $ runReader (resolveProgram prog) 
                        $ refTypes ctx
        