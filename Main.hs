import System
import IO
import Schemey.Repl

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0         -> runRepl
    1         -> runOne $ args !! 0
    otherwise -> putStrLn "Progam can take, at most, one argument."

{--
Just a reminder of some useful functions

(define (foldr f acc ls) (if (eq? ls '()) acc (f (car ls) (foldr f acc (cdr ls)))))

(define (foldl f acc ls) (if (eq? ls '()) acc (foldl f (f acc (car ls)) (cdr ls))))

(define (map f ls) (foldr (lambda (x ms) (cons (f x) ms)) '() ls))
--}