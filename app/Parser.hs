{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Text
import Text.Parsec.Char
import Text.Parsec.Text
import Text.Parsec
import Control.Monad

import Ast



parseLang :: String -> Text -> Either CompileError Program
parseLang sourceName code = case parse parseProgram sourceName code of
    Right program -> Right program
    Left error -> Left $ ParseError error

commentSpaces :: Parser ()
commentSpaces = void $ spaces >> many (comment >> spaces)
    where
        comment = do
            char '#'
            many $ noneOf "\n"

parseProgram :: Parser Program
parseProgram = many1 parseFunDef

parseFunDef :: Parser FunDef
parseFunDef = do
    commentSpaces
    string "def"
    space
    commentSpaces
    name <- parseSymbol
    commentSpaces
    char '('
    args <- parseCommaList $ parseArgDecl
    char ')'
    commentSpaces
    body <- parseStatements
    string "return"
    space
    commentSpaces
    retVal <- parseExpression
    return $ FunDef name args body retVal
    where
        parseArgDecl = 
            (,) <$> parseSymbol <* commentSpaces 
            <*> (string ":" *> commentSpaces *> parseType 
            <|> return (Unknown 0))

parseSymbol :: Parser Symbol
parseSymbol = try $ do
    first <- letter<|> char '_'
    rest <- many $ alphaNum<|>char '_'
    case first : rest of
        "return" -> parserFail "Reserved word used for symbol"
        "while" -> parserFail "Reserved word used for symbol"
        "if" -> parserFail "Reserved word used for symbol"
        "end" -> parserFail "Reserved word used for symbol"
        "else" -> parserFail "Reserved word used for symbol"
        "def" -> parserFail "Reserved word used for symbol"
        val -> return val

parseCommaList :: Parser a -> Parser [a]
parseCommaList subParser = go <|> [] <$ commentSpaces
    where
        go = do
            commentSpaces
            symbol <- subParser
            commentSpaces
            ((do
                char ','
                (symbol:) <$> parseCommaList subParser)
                <|> return [symbol])

parseStatements :: Parser [Statement]
parseStatements = do
    char '{'
    ret <- many $ commentSpaces *> parseStatement <* commentSpaces
    char '}'
    return ret

parseStatement :: Parser Statement
parseStatement = parseIf <|> parseWhile <|> try parseAssignment <|> parseExpr

parseAssignment :: Parser Statement
parseAssignment = do
    sym <- parseSymbol
    commentSpaces
    assignType <- string "=" <|> string "<-" <|> string "->"
    commentSpaces
    expr <- parseExpression
    let assignBuilder = 
            case assignType of
                "="  -> Assignment
                "<-" -> Push
                "->" -> Pop
    return $ assignBuilder sym expr

parseExpr :: Parser Statement
parseExpr = Expr <$> parseExpression

parseIf :: Parser Statement
parseIf = try $ do
    string "if"
    commentSpaces
    cond <- parseExpression
    commentSpaces
    body <- parseStatements
    elseBody <- try (do
        commentSpaces
        string "else"
        commentSpaces
        parseStatements) <|> return []
    return $ If cond body elseBody

parseWhile :: Parser Statement
parseWhile = try $ do
    string "while"
    commentSpaces
    cond <- parseExpression
    commentSpaces
    body <- parseStatements
    return $ While cond body

parseExpression :: Parser Expression
parseExpression = parseIntLit <|> parseCharLit <|> try parseFunCall <|> parseVariable

parseVariable :: Parser Expression
parseVariable = Variable <$> parseSymbol

parseIntLit :: Parser Expression
parseIntLit = do
    digits <- many1 digit
    return $ IntLit $ read digits

parseCharLit :: Parser Expression
parseCharLit = do
    char '\''
    value <- anyChar
    char '\''
    return $ CharLit value

parseFunCall :: Parser Expression
parseFunCall = do
    name <- parseSymbol
    char '('
    args <- parseCommaList parseExpression
    char ')'
    return $ Function name args

-- types

parseType = parseUnknownType 
            <|> parseIntType 
            <|> parseCharType
            <|> parseListType 
            <|> parseArrowType

parseUnknownType :: Parser Typ
parseUnknownType = do
    string "?"
    return $ Unknown 0

parseIntType :: Parser Typ
parseIntType = do
    string "nat"
    return Nat

parseCharType :: Parser Typ
parseCharType = do
    string "char"
    return Char

parseListType :: Parser Typ
parseListType = do
    string "list"
    commentSpaces
    string "("
    commentSpaces
    sub <- parseType 
    commentSpaces
    string ")"
    return $ List sub

parseArrowType :: Parser Typ
parseArrowType = do
    string "function"
    commentSpaces
    string "("
    commentSpaces
    args <- parseCommaList parseType
    commentSpaces
    string ")"
    commentSpaces
    string "->"
    commentSpaces
    retType <- parseType
    return $ Arrow args retType
