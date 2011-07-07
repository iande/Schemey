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

runOne :: [String] -> IO ()
runOne args = do
  env <- baseEnv >>= flip bindVars [("args", LList $ map LString $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (LList [LAtom "load", LString (args !! 0)]))
    >>= hPutStrLn stderr

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

