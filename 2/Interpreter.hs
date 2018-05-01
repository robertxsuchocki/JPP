-- module Interpreter where
--
-- import AbsBreve
-- import LexBreve
-- import ParBreve
-- import ErrM

import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.Maybe

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read)

data Block = Block [Stmt]
  deriving (Eq, Ord, Show, Read)

data Item = NoInit Ident | Init Ident Expr
  deriving (Eq, Ord, Show, Read)

data Expr = ELitInt Int
          | Neg Expr
          | EAdd Expr AddOp Expr
          | EMul Expr MulOp Expr
          | EVar Ident
  deriving (Eq, Ord, Show, Read)

data Stmt = Empty                   -- skip
          | BStmt Block             -- begin [S] end
          | Decl [Item]             -- var x, y = 0;
          | Ass Ident Expr          -- x := e
          | CondElse Expr Stmt Stmt -- if b then S1 else S2
          | While Expr Stmt         -- while b do S
  deriving (Eq, Ord, Show, Read)

type Loc = Int

type Env = M.Map String Loc

type Store = M.Map Loc Int

type RR a = ReaderT Env (State Store) a

data AddOp = Plus | Minus
  deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
  deriving (Eq, Ord, Show, Read)

alloc :: RR Loc
alloc = do
  st <- get
  if (M.null st)
    then return 0
    else let (i, v) = M.findMax st in return (i + 1)


eval :: Expr -> RR Int

eval (ELitInt n) = return n

eval (Neg e) = do
  v <- eval e
  return (-v)

eval (EAdd e1 op e2) = do
  v1 <- eval e1
  v2 <- eval e2
  let op' = case op of Plus  -> (+)
                       Minus -> (-)
  return (op' v1 v2)

eval (EMul e1 op e2) = do
  v1 <- eval e1
  v2 <- eval e2
  let op' = case op of Times -> (*)
                       Div   -> div
                       Mod   -> mod
  return (op' v1 v2)

eval (EVar (Ident x)) = do
  env <- ask
  st  <- get
  let l = fromMaybe (error "undefined variable") (M.lookup x env)
  return $ fromMaybe (error "undefined variable") (M.lookup l st)


interpret :: Stmt -> RR ()

interpret Empty = return ()

interpret (BStmt (Block [])) = return ()
interpret (BStmt (Block (s:ss))) = do
  interpret s
  interpret (BStmt (Block ss))

interpret (Decl [])  = return ()
interpret (Decl ((NoInit (Ident x)):ds)) = do
  l <- alloc
  local (M.insert x l) (interpret (Decl ds))
interpret (Decl ((Init (Ident x) e):ds)) = do
  l <- alloc
  v <- eval e
  modify (M.insert l v)
  local (M.insert x l) (interpret (Decl ds))

interpret (Ass (Ident x) e) = do
  env <- ask
  v <- eval e
  let l = fromMaybe (error "undefined variable") (M.lookup x env)
  modify (M.insert l v)

interpret (CondElse e s1 s2) = do
  v <- eval e
  if v /= 0
    then interpret s1
    else interpret s2

interpret (While e s) = do
  v <- eval e
  if v == 0
    then return ()
    else do
      interpret s
      interpret (While e s)

execStmt :: Stmt -> IO ()
execStmt s = mapM_ printPair $ M.toList $
              execState (runReaderT (interpret s) M.empty) M.empty
  where
    printPair :: (Int, Int) -> IO ()
    printPair (l, i) = do
      putStrLn $ (show l) ++ ", " ++ (show i)

main = execStmt testFinal

testFinal = BStmt (Block [Decl [Init (Ident "x") (ELitInt 1000)],Decl [Init (Ident "y") (ELitInt 10)],While (EVar (Ident "y")) (BStmt (Block [CondElse (EVar (Ident "x")) (BStmt (Block [Empty,Decl [Init (Ident "x") (Neg (ELitInt 1))],Ass (Ident "y") (EAdd (EVar (Ident "y")) Plus (EVar (Ident "x")))])) (BStmt (Block [Empty,Decl [Init (Ident "x") (ELitInt 1)],Ass (Ident "y") (EAdd (EVar (Ident "y")) Minus (EVar (Ident "x")))])),Ass (Ident "x") (EAdd (EVar (Ident "x")) Plus (EVar (Ident "y")))]))])
