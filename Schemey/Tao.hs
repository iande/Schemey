module Schemey.Tao
  (
    eval,
    apply
  ) where

import Schemey.Env
import Schemey.Parse (load)
import Control.Monad
import Control.Monad.Error
import IO

eval :: LEnv -> LispVal -> IOThrowsError LispVal
eval _ v@(LString _) = return v
eval _ v@(LNumber _) = return v
eval _ v@(LBool _) = return v
eval env (LAtom nm) = getVar env nm
eval _ (LList [LAtom "quote", v]) = return v
eval env (LList [LAtom "load", LString fname]) =
  load fname >>= liftM last . mapM (eval env)
eval env (LList [LAtom "if", p, q, e]) = do
  result <- eval env p
  case result of
    LBool False -> eval env e
    otherwise   -> eval env q
eval env (LList [LAtom "set!", LAtom name, form]) =
  eval env form >>= setVar env name
eval env (LList [LAtom "define", LAtom name, form]) =
  eval env form >>= defineVar env name
eval env (LList (LAtom "define" : (LList (LAtom name : params)) : body)) =
  mkNormalFunc env params body >>= defineVar env name
eval env (LList (LAtom "define" : LDottedList (LAtom name : params) varargs : body)) =
  mkVarargs varargs env params body >>= defineVar env name
eval env (LList (LAtom "lambda" : LList params : body)) =
  mkNormalFunc env params body
eval env (LList (LAtom "lambda" : LDottedList params varargs : body)) =
  mkVarargs varargs env params body
eval env (LList (LAtom "lambda" : varargs@(LAtom _) : body)) =
  mkVarargs varargs env [] body
eval env (LList (func : args)) = do
  f <- eval env func
  argVals <- mapM (eval env) args
  apply f argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc f) args = f args
apply (PrimitiveFunc f) args = liftThrows $ f args
apply (Func ps vargs body env) args =
  if num ps /= num args && vargs == Nothing
    then if num ps > num args
           then do
             newEnv <- liftIO $ bindVars env $ zip ps args
             mkPartialFunc newEnv (drop (length args) ps) body
           else throwError $ NumArgs (num ps) args
    else (liftIO $ bindVars env $ zip ps args) >>= bindVarArgs vargs >>= evalBody
  where remainingArgs = drop (length ps) args
        num = toInteger . length
        evalBody e = liftM last $ mapM (eval e) body
        bindVarArgs a e = case a of
          Just argName -> liftIO $ bindVars e [(argName, LList $ remainingArgs)]
          Nothing      -> return e

mkFunc varargs env params body = return $ Func (map show params) varargs body env
mkPartialFunc env params body = return $ Func params Nothing body env
mkNormalFunc = mkFunc Nothing
mkVarargs = mkFunc . Just . show
