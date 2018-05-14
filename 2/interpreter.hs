module Main where

import AbsBreve
import LexBreve
import ParBreve
import TypeBreve
import ErrM

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe

import System.Environment
import System.IO


type Env = M.Map String Loc

type Loc = Integer

type Store = M.Map Loc Value

type RESIO a = ReaderT Env (ErrorT String (StateT Store IO)) a

data Value = VVoid | VInt Integer | VBool Bool | VStr String | VList [Value]
           | VDict (M.Map Value Value) | VFunc Type Env [Arg] Block
  deriving (Eq, Ord)

data Flow = Normal | Returned Value | Broken | Continued


alloc :: RESIO Loc
alloc = do
  store <- get
  if (M.null store)
    then return 0
    else let (loc, value) = M.findMax store in return $ loc + 1


transProgram :: Program -> RESIO ()
transProgram (Program decls) =
  void $ transBlock (Block decls [(SExp (EApp (Ident "main") []))])


transBlock :: Block -> RESIO Flow

transBlock (Block (d:ds) ss) = do
  env_f <- transDecl d
  local (env_f) (transBlock (Block ds ss))

transBlock (Block [] (s:ss)) = do
  flow <- transStmt s
  case flow of
    Normal -> transBlock (Block [] ss)
    _      -> return flow

transBlock (Block _ []) = return Normal


transIdent :: Ident -> RESIO Value
transIdent (Ident name) = do
  env   <- ask
  store <- get
  let loc = fromMaybe 0 (M.lookup name env)
  let value = fromMaybe VVoid (M.lookup loc store)
  case value of
    VVoid -> throwError $ "uninitialized variable " ++ (show name)
    _     -> return value


transArg :: [(Arg, Value)] -> RESIO Env

transArg [] = do
  env <- ask
  return env

transArg (((Arg _ (Ident name)), value):args) = do
  loc   <- alloc
  modify (M.insert loc value)
  local (M.insert name loc) (transArg args)


transDecl :: Decl -> RESIO (Env -> Env)

transDecl (VarDecl type_ (NoInit (Ident name))) = do
  loc <- alloc
  modify (M.insert loc VVoid)
  return $ M.insert name loc

transDecl (VarDecl _ (Init (Ident name) expr)) = do
  loc   <- alloc
  value <- transExpr expr
  modify (M.insert loc value)
  return $ M.insert name loc

transDecl (FunDecl type_ (Ident name) args block) = do
  env  <- ask
  loc  <- alloc
  modify (M.insert loc (VFunc type_ (M.insert name loc env) args block))
  return $ M.insert name loc


forStep :: Loc -> Integer -> Integer -> Stmt -> RESIO Flow

forStep loc value1 value2 stmt = do
  modify (M.insert loc (VInt value1))
  flow <- transStmt stmt
  case flow of
    Returned value -> return $ Returned value
    Broken         -> return Normal
    _              -> do
      if (value1 == value2)
        then return Normal
        else do
          let diff = signum (value2 - value1)
          forStep loc (value1 + diff) value2 stmt


transStmt :: Stmt -> RESIO Flow

transStmt Empty = return Normal

transStmt (BStmt block) = do transBlock block

transStmt (Ass (Ident name) expr) = do
  env   <- ask
  value <- transExpr expr
  let loc = fromMaybe 0 (M.lookup name env)
  modify (M.insert loc value)
  return Normal

transStmt (ListAss ident@(Ident name) expr1 expr2) = do
  (VList list) <- transIdent ident
  (VInt index) <- transExpr expr1
  if index > (toInteger $ length list) || index < 0
    then throwError $ "index " ++ (show index) ++ " out of range"
    else do
      env   <- ask
      value <- transExpr expr2
      let loc   = fromMaybe 0 (M.lookup name env)
      let list' = (take (fromIntegral index) list) ++ [value]
                    ++ (drop (fromIntegral (index + 1)) list)
      modify (M.insert loc (VList list'))
      return Normal

transStmt (DictAss ident@(Ident name) expr1 expr2) = do
  (VDict dict) <- transIdent ident
  env   <- ask
  key   <- transExpr expr1
  value <- transExpr expr2
  let loc   = fromMaybe 0 (M.lookup name env)
  modify (M.insert loc (VDict (M.insert key value dict)))
  return Normal

transStmt (Ret expr) = do
  value <- transExpr expr
  return $ Returned value

transStmt (VRet) = return $ Returned VVoid

transStmt (Break) = return Broken

transStmt (Continue) = return Continued

transStmt (Cond expr stmt) = transStmt (CondElse expr stmt Empty)

transStmt (CondElse expr stmt1 stmt2) = do
  (VBool value) <- transExpr expr
  if value
    then transStmt stmt1
    else transStmt stmt2

transStmt (While expr stmt) = do
  (VBool value) <- transExpr expr
  if value
    then do
      flow <- transStmt stmt
      case flow of
        Returned value -> return $ Returned value
        Broken         -> return Normal
        _              -> transStmt (While expr stmt)
    else do return Normal

transStmt (For (Ident name) expr1 expr2 stmt) = do
  (VInt value1) <- transExpr expr1
  (VInt value2) <- transExpr expr2
  loc <- alloc
  local (M.insert name loc) (forStep loc value1 value2 stmt)
  modify (M.delete loc)
  return Normal

transStmt (SExp expr) = do
  void (transExpr expr)
  return Normal

transStmt (Print expr) = do
  value <- transExpr expr
  let out = case value of (VInt int)       -> show int
                          (VBool bool)     -> show bool
                          (VStr string)    -> string
  liftIO $ hPutStr stdout $ out
  return Normal

transStmt (PrintLn expr) = do
  transStmt (Print expr)
  transStmt (Print (EStr "\n"))

transStmt (DictDel ident@(Ident name) expr) = do
  (VDict dict) <- transIdent ident
  env <- ask
  key <- transExpr expr
  let loc = fromMaybe 0 (M.lookup name env)
  modify (M.insert loc (VDict (M.delete key dict)))
  return Normal


transExpr :: Expr -> RESIO Value

transExpr (Incr ident@(Ident name)) = do
  env <- ask
  let loc = fromMaybe 0 (M.lookup name env)
  (VInt value) <- transIdent ident
  modify (M.insert loc (VInt (value + 1)))
  return $ VInt value

transExpr (Decr ident@(Ident name)) = do
  env <- ask
  let loc = fromMaybe 0 (M.lookup name env)
  (VInt value) <- transIdent ident
  modify (M.insert loc (VInt (value - 1)))
  return $ VInt value

transExpr (EIntStr expr) = do
  (VInt value) <- transExpr expr
  return $ VStr (show value)

transExpr (EStrInt expr) = do
  (VStr value) <- transExpr expr
  return $ VInt (read value::Integer)

transExpr (ListLen ident) = do
  (VList list) <- transIdent ident
  return $ VInt (toInteger (length list))

transExpr (DictHas ident expr) = do
  (VDict dict) <- transIdent ident
  key <- transExpr expr
  return $ VBool (M.member key dict)

transExpr (EVar ident) = do
  value <- transIdent ident
  return value

transExpr (ELitInt integer) = return $ VInt integer

transExpr (ELitTrue) = return $ VBool True

transExpr (ELitFalse) = return $ VBool False

transExpr (EApp ident@(Ident name) exprs) = do
  store  <- get
  values <- mapM transExpr exprs
  (VFunc type_ env args block) <- transIdent ident

  env'   <- local (\_ -> env) (transArg (zip args values))
  flow   <- local (\_ -> env') (transBlock block)

  store' <- get
  modify (\_ -> M.intersection store' store)

  case flow of
    (Returned value) -> return value
    _                -> do
      if (type_ /= Void)
        then throwError $ "function " ++ (show name) ++
          " of non-void type didn't return value"
        else return VVoid

transExpr (EStr string) = return $ VStr string

transExpr (ENewList) = return $ VList []

transExpr (EValList ident expr) = do
  (VList list) <- transIdent ident
  (VInt index) <- transExpr expr
  if index >= (toInteger $ length list)
    then throwError $ "index " ++ (show index) ++ " out of range"
    else return $ list !! (fromIntegral index)

transExpr (ENewDict) = return $ VDict M.empty

transExpr (EValDict ident expr) = do
  (VDict dict) <- transIdent ident
  key <- transExpr expr
  case (M.lookup key dict) of
    (Just value) -> return value
    _            -> do
      let msg_key = case key of (VInt int)   -> show int
                                (VBool bool) -> show bool
                                (VStr str)   -> str
      throwError $ "key " ++ msg_key ++ " not in dict"

transExpr (Neg expr) = do
  (VInt value) <- transExpr expr
  return $ VInt (-value)

transExpr (Not expr) = do
  (VBool value) <- transExpr expr
  return $ VBool (not value)

transExpr (EMul expr1 mulop expr2) = do
  (VInt value1) <- transExpr expr1
  (VInt value2) <- transExpr expr2
  op <- transMulOp mulop
  case op of
    div -> do
      if (value2 == 0)
        then throwError "division by zero"
        else return $ VInt (op value1 value2)
  return $ VInt (op value1 value2)

transExpr (EAdd expr1 addop expr2) = do
  (VInt value1) <- transExpr expr1
  (VInt value2) <- transExpr expr2
  op <- transAddOp addop
  return $ VInt (op value1 value2)

transExpr (ERel expr1 relop expr2) = do
  (VInt value1) <- transExpr expr1
  (VInt value2) <- transExpr expr2
  op <- transRelOp relop
  return $ VBool (op value1 value2)

transExpr (EAnd expr1 expr2) = do
  (VBool value1) <- transExpr expr1
  (VBool value2) <- transExpr expr2
  return $ VBool ((&&) value1 value2)

transExpr (EOr expr1 expr2) = do
  (VBool value1) <- transExpr expr1
  (VBool value2) <- transExpr expr2
  return $ VBool ((||) value1 value2)


transAddOp :: AddOp -> RESIO (Integer -> Integer -> Integer)
transAddOp x = case x of
  Plus  -> return (+)
  Minus -> return (-)


transMulOp :: MulOp -> RESIO (Integer -> Integer -> Integer)
transMulOp x = case x of
  Times -> return (*)
  Div   -> return div
  Mod   -> return mod


transRelOp :: RelOp -> RESIO (Integer -> Integer -> Bool)
transRelOp x = case x of
  LTH -> return (<)
  LE  -> return (<=)
  GTH -> return (>)
  GE  -> return (>=)
  EQU -> return (==)
  NE  -> return (/=)


runProg :: Program -> IO ()
runProg prog = do
  res <- runStateT (runErrorT (runReaderT (transProgram prog) M.empty)) M.empty
  case res of
    ((Left message), _) -> do hPutStr stderr $ "Error: " ++ message ++ "\n"
    _                   -> do return ()


main :: IO ()
main = do
  args <- getArgs
  let contents = case args of []    -> getContents
                              (f:_) -> readFile f
  code <- contents
  let prog = pProgram (myLexer code)
  case prog of
    (Ok tree) -> do
      valid <- runReaderT (validProgram tree) M.empty
      if (not valid)
        then do return ()
        else do runProg tree
    _ -> hPutStr stderr $ "Error: Parse failed\n"
