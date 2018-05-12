module Main where

import AbsBreve
import LexBreve
import ParBreve
import TypeBreve
import ErrM

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.Maybe

import System.IO

data Value = VNothing | VVoid | VInt Integer | VBool Bool
           | VString String | VFunc Env [Arg] Block
  deriving (Eq, Ord, Show, Read)

data Flow = Normal | Returned Value | Broke | Continued

type Op a b = a -> a -> b

type Env = M.Map String Loc

type Loc = Integer

type Store = M.Map Loc Value

type RESIO a = ReaderT Env (ExceptT String (StateT Store IO)) a

alloc :: RESIO Loc
alloc = do
  store <- get
  if (M.null store)
    then return 0
    else let (loc, value) = M.findMax store in return (loc + 1)

forStep :: Loc -> Integer -> Integer -> Stmt -> RESIO ()
forStep loc value1 value2 stmt = do
  modify (M.insert loc (VInt value1))
  transStmt stmt
  returned <- checkReturnStatus
  if (value1 == value2) || returned
    then return ()
    else do
      let diff = signum (value2 - value1)
      forStep loc (value1 + diff) value2 stmt

checkReturnStatus :: RESIO Bool
checkReturnStatus = do
  env   <- ask
  store <- get
  let loc   = fromMaybe 0 (M.lookup "return" env)
  let value = fromMaybe VNothing (M.lookup loc store)
  case value of
    VNothing -> return False
    _        -> return True


transIdent :: Ident -> RESIO Value
transIdent (Ident ident) = do
  env   <- ask
  store <- get
  let loc = fromMaybe 0 (M.lookup ident env)
  case (M.lookup loc store) of
    (Just value) -> return value
    _            -> throwError $ "uninitialized variable " ++ (show ident)


transProgram :: Program -> RESIO ()
transProgram (Program (topdef:_)) = do
  loc <- alloc
  modify (M.insert loc VNothing)
  local (M.insert "return" loc) (transTopDef topdef)


transTopDef :: TopDef -> RESIO ()
transTopDef (FnDef _ _ _ block) = do
  transBlock block


transArg :: [(Arg, Value)] -> RESIO Env

transArg [] = do
  env <- ask
  return env

transArg (((Arg _ (Ident ident)), value):args) = do
  loc   <- alloc
  modify (M.insert loc value)
  local (M.insert ident loc) (transArg args)


transBlock :: Block -> RESIO ()

transBlock (Block (d:ds) ss) = do
  env_f <- transDecl d
  local (env_f) (transBlock (Block ds ss))

transBlock (Block [] (s:ss)) = do
  transStmt s
  returned <- checkReturnStatus
  if (not returned)
    then transBlock (Block [] ss)
    else return ()

transBlock (Block _ []) = return ()


transStmt :: Stmt -> RESIO ()

transStmt Empty = return ()

transStmt (BStmt block) = do transBlock block

transStmt (Ass (Ident ident) expr) = do
  env   <- ask
  value <- transExpr expr
  let loc = fromMaybe 0 (M.lookup ident env)
  modify (M.insert loc value)

transStmt (Ret expr) = do transStmt (Ass (Ident "return") expr)

transStmt (VRet) = do
  env   <- ask
  let loc = fromMaybe 0 (M.lookup "return" env)
  modify (M.insert loc VVoid)

transStmt (Print expr) = do
  value <- transExpr expr
  let out = case value of (VInt int)       -> show int
                          (VBool bool)     -> show bool
                          (VString string) -> string
  liftIO $ hPutStr stdout $ out

transStmt (PrintLn expr) = do
  transStmt (Print expr)
  transStmt (Print (EString "\n"))


transStmt (Cond expr stmt) = do transStmt (CondElse expr stmt Empty)

transStmt (CondElse expr stmt1 stmt2) = do
  (VBool value) <- transExpr expr
  if value
    then transStmt stmt1
    else transStmt stmt2

transStmt (While expr stmt) = do
  (VBool value) <- transExpr expr
  if value
    then do
      transStmt stmt
      returned <- checkReturnStatus
      if (not returned)
        then transStmt (While expr stmt)
        else do return ()
    else do return ()

transStmt (For (Ident ident) expr1 expr2 stmt) = do
  (VInt value1) <- transExpr expr1
  (VInt value2) <- transExpr expr2
  loc <- alloc
  local (M.insert ident loc) (forStep loc value1 value2 stmt)
  modify (M.delete loc)

transStmt (SExp expr) = do void (transExpr expr)

transStmt x = case x of
  ArrayAss ident expr1 expr2 -> return ()
  DictAss ident expr1 expr2 -> return ()


transDecl :: Decl -> RESIO (Env -> Env)

transDecl (VarDecl _ (NoInit (Ident ident))) = do
  loc <- alloc
  modify (M.insert loc VVoid)
  return (M.insert ident loc)

transDecl (VarDecl _ (Init (Ident ident) expr)) = do
  loc   <- alloc
  value <- transExpr expr
  modify (M.insert loc value)
  return (M.insert ident loc)

transDecl (FunDecl _ (Ident ident) args block) = do
  env  <- ask
  loc  <- alloc
  modify (M.insert loc (VFunc (M.insert ident loc env) args block))
  return (M.insert ident loc)


transType :: Type -> RESIO ()
transType x = case x of
  Int -> return ()
  Str -> return ()
  Bool -> return ()
  Void -> return ()
  Array type_ -> return ()
  Dict type_1 type_2 -> return ()
  Fun type_ types -> return ()


transExpr :: Expr -> RESIO Value

transExpr (Incr (Ident ident)) = do
  env <- ask
  let loc = fromMaybe 0 (M.lookup ident env)
  (VInt value) <- transIdent (Ident ident)
  modify (M.insert loc (VInt (value + 1)))
  return (VInt value)

transExpr (Decr (Ident ident)) = do
  env <- ask
  let loc = fromMaybe 0 (M.lookup ident env)
  (VInt value) <- transIdent (Ident ident)
  modify (M.insert loc (VInt (value - 1)))
  return (VInt value)

transExpr (EIntStr expr) = do
  (VInt value) <- transExpr expr
  return (VString (show value))

transExpr (EStrInt expr) = do
  (VString value) <- transExpr expr
  return (VInt (read value::Integer))

transExpr (EVar ident) = do
  value <- transIdent ident
  return value

transExpr (ELitInt integer) = return (VInt integer)

transExpr (ELitTrue) = return (VBool True)

transExpr (ELitFalse) = return (VBool False)

transExpr (EApp ident exprs) = do
  store  <- get
  values <- mapM transExpr exprs
  (VFunc env args block) <- transIdent ident

  env'   <- local (\_ -> env) (transArg (zip args values))
  local (\_ -> env') (transBlock block)

  store' <- get
  modify (\_ -> M.intersection store' store)

  let loc   = fromMaybe 0 (M.lookup "return" env)
  let value = fromMaybe VVoid (M.lookup loc store')
  modify (M.insert loc VNothing)
  return value

transExpr (EString string) = return (VString string)

transExpr (Neg expr) = do
  (VInt value) <- transExpr expr
  return (VInt (-value))

transExpr (Not expr) = do
  (VBool value) <- transExpr expr
  return (VBool (not value))

transExpr (EMul expr1 mulop expr2) = do
  (VInt value1) <- transExpr expr1
  (VInt value2) <- transExpr expr2
  op <- transMulOp mulop
  case op of
    div -> do
      if (value2 == 0)
        then do throwError "division by zero"
        else do return (VInt (op value1 value2))
  do return (VInt (op value1 value2))

transExpr (EAdd expr1 addop expr2) = do
  (VInt value1) <- transExpr expr1
  (VInt value2) <- transExpr expr2
  op <- transAddOp addop
  return (VInt (op value1 value2))

transExpr (ERel expr1 relop expr2) = do
  (VInt value1) <- transExpr expr1
  (VInt value2) <- transExpr expr2
  op <- transRelOp relop
  return (VBool (op value1 value2))

transExpr (EAnd expr1 expr2) = do
  (VBool value1) <- transExpr expr1
  (VBool value2) <- transExpr expr2
  return (VBool ((&&) value1 value2))

transExpr (EOr expr1 expr2) = do
  (VBool value1) <- transExpr expr1
  (VBool value2) <- transExpr expr2
  return (VBool ((||) value1 value2))

transExpr x = case x of
  ENewArray -> return VNothing
  EValArray ident expr -> return VNothing
  ENewDict -> return VNothing
  EValDict ident expr -> return VNothing


transAddOp :: AddOp -> RESIO (Op Integer Integer)
transAddOp x = case x of
  Plus  -> return (+)
  Minus -> return (-)


transMulOp :: MulOp -> RESIO (Op Integer Integer)
transMulOp x = case x of
  Times -> return (*)
  Div   -> return div
  Mod   -> return mod


transRelOp :: RelOp -> RESIO (Op Integer Bool)
transRelOp x = case x of
  LTH -> return (<)
  LE  -> return (<=)
  GTH -> return (>)
  GE  -> return (>=)
  EQU -> return (==)
  NE  -> return (/=)


runProg :: Program -> IO ()
runProg prog = do
  (ex, _) <- runStateT (runExceptT (runReaderT (transProgram prog) M.empty)) M.empty
  case ex of
    (Left message) -> do hPutStr stderr $ "Error: " ++ message ++ "\n"
    _              -> do return ()


main :: IO ()
main = do
  code <- getContents
  let prog = pProgram (myLexer code)
  case prog of
    (Ok tree) -> do
      valid <- runReaderT (validProgram tree) M.empty
      if (not valid)
        then do return ()
        else do runProg tree
    _ -> hPutStr stderr $ "Error: Parse failed"
