module Schemey.Repl
  (
    runOne,
    runRepl
  ) where

import Schemey.Env
import Schemey.Parse
import Schemey.Primitives (baseEnv)
import Schemey.Tao
import IO
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec

runOne :: String -> IO ()
runOne expr = baseEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = baseEnv >>= until_ (== "quit") (readPrompt "L> ") . evalAndPrint

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt p = flushStr p >> getLine

evalString :: LEnv -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: LEnv -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ cond prompt act = do
  result <- prompt
  if cond result
    then return ()
    else act result >> until_ cond prompt act

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val
