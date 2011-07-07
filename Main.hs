import System
import IO
import Schemey.Repl

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne $ args
