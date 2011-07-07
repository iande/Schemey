module Schemey.Env
  (
    LEnv (..),
    LispError (..),
    LispVal (..),
    ThrowsError (..),
    IOThrowsError (..),
    defineVar,
    setVar,
    getVar,
    bindVars,
    liftThrows,
    runIOThrows
  ) where

import IO
import Data.IORef
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

data LispVal = LAtom String
             | LList [LispVal]
             | LDottedList [LispVal] LispVal
             | LNumber Integer
             | LString String
             | LBool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {
                 params :: [String],
                 vararg :: (Maybe String),
                 body :: [LispVal],
                 closure :: LEnv
               }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO
type LEnv = IORef [(String, IORef LispVal)]

instance Show LispVal where show = showVal
instance Show LispError where show = showError
instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

isBound :: LEnv -> String -> IO Bool
isBound env name = readIORef env >>= return . maybe False (const True) . lookup name

getVar :: LEnv -> String -> IOThrowsError LispVal
getVar env name = do
  ns <- liftIO $ readIORef env
  maybe (throwError $ UnboundVar "Getting an unbound variable: " name)
        (liftIO . readIORef)
        (lookup name ns)

setVar :: LEnv -> String -> LispVal -> IOThrowsError LispVal
setVar env name val = do
  ns <- liftIO $ readIORef env
  maybe (throwError $ UnboundVar "Setting an unbound variable: " name)
        (liftIO . (flip writeIORef val))
        (lookup name ns)
  return val

defineVar :: LEnv -> String -> LispVal -> IOThrowsError LispVal
defineVar env name val = do
  alreadyDefined <- liftIO $ isBound env name
  if alreadyDefined
    then setVar env name val >> return val
    else liftIO $ do
      valRef <- newIORef val
      ns <- readIORef env
      writeIORef env ((name, valRef) : ns)
      return val

bindVars :: LEnv -> [(String, LispVal)] -> IO LEnv
bindVars env bindings = readIORef env >>= extendEnv bindings >>= newIORef
  where extendEnv bindings ns = liftM (++ ns) (mapM addBinding bindings)
        addBinding (name, val) = do
          ref <- newIORef val
          return (name, ref)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right v) = v

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"
showVal (LString str) = "\"" ++ str ++ "\""
showVal (LAtom name) = name
showVal (LNumber n) = show n
showVal (LBool True) = "#t"
showVal (LBool False) = "#f"
showVal (LList vs) = "(" ++ (unwordsList vs) ++ ")"
showVal (LDottedList h t) = "(" ++ (unwordsList h) ++ " . " ++ showVal t ++ ")"
showVal (PrimitiveFunc _) = "<primitive function>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"


showError :: LispError -> String
showError (UnboundVar m v) = m ++ ": " ++ v
showError (BadSpecialForm m f) = m ++ ": " ++ show f
showError (NotFunction m f) = m ++ ": " ++ show f
showError (NumArgs e g) = "Expected " ++ show e ++ " args; found values "
                       ++ unwordsList g
showError (TypeMismatch e g) = "Invalid type: expected " ++ e
                            ++ ", found " ++ show g
showError (Parser p) = "Parse error at " ++ show p
