module Main where

import System.Environment
import qualified Data.Text.IO as T
import Control.Monad.Trans.State

import Parser
import TypeInfer
import qualified Ast

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [] -> putStrLn "Input file required"
        x : xs -> do 
            code <- T.readFile x
            case parseLang x code of
                Left err -> print err
                Right ast -> do
                    -- print ast
                    case typeCheck ast of
                        Left err -> print err
                        Right functions -> do
                            putStrLn "Sucessfully typechecked!"
                            mapM_ (putStrLn . Ast.showFunType) functions