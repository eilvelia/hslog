module Main where

import System.IO (hFlush, stdout, readFile)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Control.Monad (forever)
import Data.Bifunctor
import Data.List (intercalate)
import Text.Megaparsec
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Hslog.Types
import qualified Hslog.Parser as P
import Hslog.Solver

(|>) :: a -> (a -> r) -> r
x |> f = f x

exceptT :: (Monad m) => Either e a -> ExceptT e m a
exceptT = ExceptT . return

-- repl without l
repl' :: KnowledgeBase -> ExceptT String IO ()
repl' kb = do
  lift $ putStr "?- "
  lift $ hFlush stdout
  input <- lift getLine
  ast <- exceptT $ parse P.query "repl" input
    |> first (\err -> "ParseError: " ++ parseErrorPretty err)
  lift $ putStrLn $ "  AST: " ++ show ast
  substs <- exceptT $ solveL kb ast
    |> first (\err -> "SolveError: " ++ show err)
  let
    substsStr =
      intercalate ", "
      . map (\(k, v) -> gen k ++ " = " ++ gen v)
      $ substs
    result = (if substsStr == "" then "true" else substsStr) ++ "."
  lift $ putStrLn result

repl :: KnowledgeBase -> IO ()
repl kb = forever $ do
  result <- runExceptT $ repl' kb
  either putStrLn (\_ -> return ()) result

main :: IO ()
main = do
  args <- getArgs
  let filename' = case args of { [] -> Nothing ; x:_ -> Just x }
  case filename' of
    Just filename -> do
      putStrLn $ "- Reading file '" ++ filename ++ "'."
      content <- readFile filename
      case parse P.file filename content of
        Right clauses -> repl (kbFromList clauses)
        Left err -> do
          putStrLn $ parseErrorPretty err
          exitWith (ExitFailure 1)
    Nothing ->
      repl emptyKb
