module TypeInfer where

import Control.Monad.Trans.State
import Control.Monad

import Ast

matchTyp :: Typ -> Typ -> Maybe Typ
matchTyp Unknown b = Just b
matchTyp a Unknown = Just a
matchTyp Byte Byte = Just Byte
matchTyp (List a) (List b) = do
    sub <- matchTyp a b
    return $ List sub 

matchTyp (Arrow args1 ret1) (Arrow args2 ret2) = do 
    ret <- matchTyp ret1 ret2
    args <- mapM (uncurry matchTyp) $ zip args1 args2
    return $ Arrow args ret

matchTyp (UnknownFun a) (UnknownFun b) = UnknownFun <$> matchTyp a b
matchTyp (UnknownFun a) (Arrow bs b) = Arrow bs <$> matchTyp a b
matchTyp (Arrow as a) (UnknownFun b) = Arrow as <$> matchTyp a b

matchTyp _ _ = Nothing

known :: Typ -> Bool
known Unknown = False
known (UnknownFun _) = False
known Byte = True

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

contextEntryUnknown FrameSep = False
contextEntryUnknown (Def _ typ) = not $ known typ

type TypeCheckContext = [TypeCheckContextEntry]

type TypeCheckEnvironment = StateT TypeCheckContext (Either TypeCheckError)

failTC :: TypeCheckError -> TypeCheckEnvironment a
failTC = StateT . const . Left 

lookupMatchOrAdd :: Symbol -> Typ -> TypeCheckEnvironment Typ
lookupMatchOrAdd sym typ = do
    ctx <- get
    (ctx', ret) <- 
        case go ctx id of 
            Right a -> return $ a
            Left err -> failTC $ err
    put ctx'
    return ret
    where
        go :: TypeCheckContext -> (TypeCheckContext -> TypeCheckContext) -> Either TypeCheckError (TypeCheckContext, Typ)
        go [] rebuild = Right (Def sym typ : rebuild [], typ)
        go ((Def s t):xs) rebuild
            | s == sym = case matchTyp typ t of 
                Just res -> Right $ (rebuild $ (Def sym res) : xs, res)
                Nothing -> Left $ TypeMismatch typ t $ "symbol \"" ++ sym ++"\""
            | otherwise = go xs (rebuild . ((Def s t):) )
        go (a:xs) rebuild = go xs (rebuild . (a:))
        

---

typeCheckExpr :: Typ -> Expression -> TypeCheckEnvironment Typ
typeCheckExpr typ (Variable s) = lookupMatchOrAdd s typ

typeCheckExpr typ (IntLit _) =
    case matchTyp typ Byte of
        Just a -> return a
        Nothing -> failTC $ TypeMismatch Byte typ "Number literal"

typeCheckExpr typ (CharLit _) =
    case matchTyp typ Byte of
        Just a -> return a
        Nothing -> failTC $ TypeMismatch Byte typ "Character literal"

typeCheckExpr typ (Function name args) = do
    funtyp <- lookupMatchOrAdd name (UnknownFun typ)
    argTypes <-
        case funtyp of 
            UnknownFun _ -> mapM (typeCheckExpr Unknown) args
            Arrow expectedArgs _ -> do
                let 
                    nexp = length expectedArgs
                    ngiv = length args
                when (nexp /= ngiv) $ 
                    failTC $ ArgCount name nexp ngiv
                zipWithM typeCheckExpr expectedArgs args
            _ -> failTC $ TypeType funtyp "Not a function type"
    res <- lookupMatchOrAdd name (Arrow argTypes Unknown)
    case res of
        UnknownFun a -> return a
        Arrow _ a -> return a
        _ -> failTC $ TypeType res "Not a function type 2"

---

typeCheckStatements :: [Statement] -> TypeCheckEnvironment ()
typeCheckStatements stmnts = const () <$> mapM typeCheckStatement stmnts

typeCheckStatement :: Statement -> TypeCheckEnvironment () 
typeCheckStatement (Assignment sym expr) = do
    typ <- lookupMatchOrAdd sym Unknown
    typ <- typeCheckExpr typ expr
    lookupMatchOrAdd sym typ
    return ()

typeCheckStatement (Push sym expr) = do
    lsttyp <- lookupMatchOrAdd sym (List Unknown)
    let typ = case lsttyp of List a -> a
    typ <- typeCheckExpr typ expr
    lookupMatchOrAdd sym (List typ)
    return ()

typeCheckStatement (Pop sym expr) = do
    lsttyp <- lookupMatchOrAdd sym (List Unknown)
    let typ = case lsttyp of List a -> a
    typ <- typeCheckExpr typ expr
    lookupMatchOrAdd sym (List typ)
    return ()

typeCheckStatement (Expr expr) = do
    typeCheckExpr Unknown expr
    return ()

typeCheckStatement (If expr stmnts1 stmnts2) = do
    typeCheckExpr Byte expr
    typeCheckStatements stmnts1
    typeCheckStatements stmnts2

typeCheckStatement (While expr stmts) = do 
    typeCheckExpr Byte expr
    typeCheckStatements stmts

---

typeCheckFunDefs :: [FunDef] -> TypeCheckEnvironment ()
typeCheckFunDefs defs = const () <$> mapM typeCheckFunDef defs

typeCheckFunDef :: FunDef -> TypeCheckEnvironment ()
typeCheckFunDef (FunDef name args body returnValue) = do
    modify pushFrame
    mapM (uncurry lookupMatchOrAdd) args
    typeCheckStatements body
    retType <- typeCheckExpr Unknown returnValue
    argTypes <- mapM (uncurry lookupMatchOrAdd) args
    ctx <- get
    case filter contextEntryUnknown ctx of
        [] -> return ()
        Def sym typ:_ -> failTC $ Unresolved sym typ
        _ -> undefined
    put $ popFrame ctx
    lookupMatchOrAdd name (Arrow argTypes retType)
    return ()
    where 
        popFrame (FrameSep:xs) = xs
        popFrame (x:xs) = popFrame xs
        popFrame [] = []

        pushFrame xs = FrameSep:xs

---
initialContext = [
    Def "in" $ Arrow [] Byte, 
    Def "out" $ Arrow [Byte] Byte, -- no void type yet
    Def "inc" $ Arrow [Byte] Byte,
    Def "dec" $ Arrow [Byte] Byte
    ]

typeCheck :: Program -> Maybe TypeCheckError
typeCheck prog = 
    case execStateT (typeCheckFunDefs prog) initialContext  of
        Left err -> Just err
        Right ctx -> case filter contextEntryUnknown ctx of
            [] -> Nothing
            Def n t:xs -> Just $ Unresolved n t
            FrameSep:xs -> undefined --should be impossible 
        