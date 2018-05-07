module Main where

import LexBreve
import ParBreve
import AbsBreve
import ErrM

import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.Maybe

type Loc = Integer

type Env = M.Map String Loc

type Store = M.Map Loc Integer

type RR a = ReaderT Env (State Store) a

alloc :: RR Loc
alloc = do
  st <- get
  if (M.null st)
    then return 0
    else let (i, v) = M.findMax st in return (i + 1)


transExpr :: Expr -> RR Integer

transExpr (ELitInt n) = return n

transExpr (Neg e) = do
  v <- transExpr e
  return (-v)

transExpr (EAdd e1 op e2) = do
  v1 <- transExpr e1
  v2 <- transExpr e2
  let op' = case op of Plus  -> (+)
                       Minus -> (-)
  return (op' v1 v2)

transExpr (EMul e1 op e2) = do
  v1 <- transExpr e1
  v2 <- transExpr e2
  let op' = case op of Times -> (*)
                       Div   -> div
                       Mod   -> mod
  return (op' v1 v2)

transExpr (EVar (Ident x)) = do
  env <- ask
  st  <- get
  let l = fromMaybe (error "undefined variable") (M.lookup x env)
  return $ fromMaybe (error "undefined variable") (M.lookup l st)


transStmt :: Stmt -> RR ()

transStmt Empty = return ()

transStmt (BStmt (Block _ [])) = return ()
transStmt (BStmt (Block [] (s:ss))) = do
  transStmt s
  transStmt (BStmt (Block [] ss))

transStmt (BStmt (Block ((VarDecl t (NoInit (Ident x))):ds) ss)) = do
  l <- alloc
  local (M.insert x l) (transStmt (BStmt (Block ds ss)))

transStmt (BStmt (Block ((VarDecl t (Init (Ident x) e)):ds) ss)) = do
  l <- alloc
  v <- transExpr e
  modify (M.insert l v)
  local (M.insert x l) (transStmt (BStmt (Block ds ss)))

transStmt (Ass (Ident x) e) = do
  env <- ask
  v <- transExpr e
  let l = fromMaybe (error "undefined variable") (M.lookup x env)
  modify (M.insert l v)

transStmt (CondElse e s1 s2) = do
  v <- transExpr e
  if v /= 0
    then transStmt s1
    else transStmt s2

transStmt (While e s) = do
  v <- transExpr e
  if v == 0
    then return ()
    else do
      transStmt s
      transStmt (While e s)

transProgram :: Program -> RR ()
transProgram (Program topdefs) = do
  transTopDef topdefs

transTopDef :: [TopDef] -> RR ()
transTopDef ((FnDef Int (Ident "main") [] b):ds) = do
  transStmt (BStmt b)

execProg :: Program -> String
execProg s = concat $ map printPair $ M.toList $
              execState (runReaderT (transProgram s) M.empty) M.empty
  where
    printPair :: (Integer, Integer) -> String
    printPair (l, i) = (show l) ++ ", " ++ (show i) ++ "\n"

main = do
  interact breve
  putStrLn ""

breve s =
  let Ok e = pProgram (myLexer s)
  in (execProg e)
