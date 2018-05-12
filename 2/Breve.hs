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

data Flow = Normal | Returned Value | Broken | Continued
  deriving (Eq, Ord, Show, Read)

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

forStep :: Loc -> Integer -> Integer -> Stmt -> RESIO Flow
forStep loc value1 value2 stmt = do
  modify (M.insert loc (VInt value1))
  flow <- transStmt stmt
  case flow of
    Returned value -> return (Returned value)
    Broken         -> return Normal
    _              -> do
      if (value1 == value2)
        then return Normal
        else do
          let diff = signum (value2 - value1)
          forStep loc (value1 + diff) value2 stmt


transIdent :: Ident -> RESIO Value
transIdent (Ident ident) = do
  env   <- ask
  store <- get
  let loc = fromMaybe 0 (M.lookup ident env)
  case (M.lookup loc store) of
    (Just value) -> return value
    _            -> throwError $ "uninitialized variable " ++ (show ident)


transProgram :: Program -> RESIO ()
transProgram (Program decls) =
  void $ transBlock (Block decls [(SExp (EApp (Ident "main") []))])


transArg :: [(Arg, Value)] -> RESIO Env

transArg [] = do
  env <- ask
  return env

transArg (((Arg _ (Ident ident)), value):args) = do
  loc   <- alloc
  modify (M.insert loc value)
  local (M.insert ident loc) (transArg args)


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


transStmt :: Stmt -> RESIO Flow

transStmt Empty = return Normal

transStmt (BStmt block) = do transBlock block

transStmt (Ass (Ident ident) expr) = do
  env   <- ask
  value <- transExpr expr
  let loc = fromMaybe 0 (M.lookup ident env)
  modify (M.insert loc value)
  return Normal

transStmt (Ret expr) = do
  value <- transExpr expr
  return (Returned value)

transStmt (VRet) = return (Returned VVoid)

transStmt (Break) = return Broken

transStmt (Continue) = return Continued

transStmt (Print expr) = do
  value <- transExpr expr
  let out = case value of (VInt int)       -> show int
                          (VBool bool)     -> show bool
                          (VString string) -> string
  liftIO $ hPutStr stdout $ out
  return Normal

transStmt (PrintLn expr) = do
  transStmt (Print expr)
  transStmt (Print (EString "\n"))


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
        Returned value -> return (Returned value)
        Broken         -> return Normal
        _              -> transStmt (While expr stmt)
    else do return Normal

transStmt (For (Ident ident) expr1 expr2 stmt) = do
  (VInt value1) <- transExpr expr1
  (VInt value2) <- transExpr expr2
  loc <- alloc
  local (M.insert ident loc) (forStep loc value1 value2 stmt)
  modify (M.delete loc)
  return Normal

transStmt (SExp expr) = do
  void (transExpr expr)
  return Normal

transStmt x = case x of
  ArrayAss ident expr1 expr2 -> return Normal
  DictAss ident expr1 expr2 -> return Normal


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
  flow   <- local (\_ -> env') (transBlock block)

  store' <- get
  modify (\_ -> M.intersection store' store)

  case flow of
    (Returned value) -> return value
    _                -> return VVoid

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
    _ -> hPutStr stderr $ "Error: Parse failed\n"
