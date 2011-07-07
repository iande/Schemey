{-# LANGUAGE ExistentialQuantification #-}
module Schemey.Primitives
  (
    nullEnv,
    baseEnv
  ) where

import Schemey.Env
import Control.Monad
import Control.Monad.Error
import Data.IORef

numericUniop :: (Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericUniop op [x] = do
  val <- unpackNum x
  return $ LNumber (op val)
numericUniop op badArgs = throwError $ NumArgs 1 badArgs

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [x]  = do
  val <- unpackNum x
  return $ PrimitiveFunc (numericUniop (op val))
numericBinop op params = mapM unpackNum params >>= return . LNumber . foldl1 op

boolUniop :: (LispVal -> ThrowsError a) -> (a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolUniop un op [x] = do
  val <- un x
  return $ LBool $ (op val)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop un op args = if length args > 2
                       then throwError $ NumArgs 2 args
                       else if length args == 1
                            then do
                              left <- un $ args !! 0
                              return $ PrimitiveFunc (boolUniop un (op left))
                            else do
                              left  <- un $ args !! 0
                              right <- un $ args !! 1
                              return $ LBool $ (op left right)

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool 

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (LNumber n) = return n
unpackNum (LString n) = let parsed = reads n in 
                          if null parsed 
                            then throwError $ TypeMismatch "number" $ LString n
                            else return $ fst $ parsed !! 0
unpackNum (LList [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (LBool b) = return b
unpackBool notBool   = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (LString s) = return s
unpackStr (LBool b)   = return $ show b
unpackStr (LNumber n) = return $ show n
unpackStr notStr      = throwError $ TypeMismatch "string" notStr

-- list primitives
car :: [LispVal] -> ThrowsError LispVal
car [(LList (x : xs))] = return x
car [(LDottedList (x : xs) _)] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgs = throwError $ NumArgs 1 badArgs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [(LList (x : xs))] = return $ LList xs
cdr [(LDottedList [_] t)] = return t
cdr [(LDottedList (_:xs) t)] = return $ LDottedList xs t
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgs = throwError $ NumArgs 1 badArgs

consTo :: LispVal -> [LispVal] -> ThrowsError LispVal
consTo x [LList []] = return $ LList [x]
consTo x [LList xs] = return $ LList (x : xs)
consTo x [LDottedList xs t] = return $ LDottedList (x : xs) t
consTo x [y] = return $ LDottedList [x] y

cons :: [LispVal] -> ThrowsError LispVal
cons [x] = return $ PrimitiveFunc (consTo x)
cons (x:ys) = consTo x ys
-- cons [x, LList []] = return $ LList [x]
-- cons [x, LList xs] = return $ LList (x : xs)
-- cons [x, LDottedList xs t] = return $ LDottedList (x : xs) t
-- cons [x, y] = return $ LDottedList [x] y
cons badArgs = throwError $ NumArgs 2 badArgs

eqvTo :: LispVal -> [LispVal] -> ThrowsError LispVal
eqvTo (LBool x) [(LBool y)]                   = return $ LBool $ x == y
eqvTo (LNumber x) [(LNumber y)]               = return $ LBool $ x == y
eqvTo (LString x) [(LString y)]               = return $ LBool $ x == y
eqvTo (LAtom x)   [(LAtom y)]                 = return $ LBool $ x == y
eqvTo (LDottedList xs x) [(LDottedList ys y)] = eqvTo (LList $ xs ++ [x]) [LList $ ys ++ [y]]
eqvTo (LList x) [(LList y)]                   = return $ LBool $
                                                 (length x == length y) &&
                                                 (all eqvPair $ zip x y)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err          -> False
                               Right (LBool val) -> val
eqvTo _ [_]                                   = return $ LBool False

eqv :: [LispVal] -> ThrowsError LispVal
eqv [x] = return $ PrimitiveFunc (eqvTo x)
eqv (x:ys) = eqvTo x ys
eqv badArgs = throwError $ NumArgs 2 badArgs

-- Now for weak equivalence checking, we need some polymorphic list action
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker un) = do
  unx <- un x
  uny <- un y
  return $ unx == uny
  `catchError` (const $ return False)

equalTo :: LispVal -> [LispVal] -> ThrowsError LispVal
equalTo x [y] = do
  primEq <- liftM or $ mapM (unpackEquals x y)
            [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEq <- eqvTo x [y]
  return $ LBool $ (primEq || let (LBool x) = eqvEq in x)

equal :: [LispVal] -> ThrowsError LispVal
equal [x] = return $ PrimitiveFunc (equalTo x)
equal (x:ys) = equalTo x ys
equal badArgs = throwError $ NumArgs 2 badArgs

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

nullEnv :: IO LEnv
nullEnv = newIORef []

baseEnv :: IO LEnv
baseEnv = nullEnv >>= (flip bindVars $ map mkPrimitiveFunc primitives)
  where mkPrimitiveFunc (name, func) = (name, PrimitiveFunc func)