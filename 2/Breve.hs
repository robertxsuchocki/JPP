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


transIdent :: Ident -> RR ()
transIdent x = case x of
  Ident string -> return ()


transProgram :: Program -> RR ()
transProgram (Program (topdef:_)) = do
  transTopDef topdef


transTopDef :: TopDef -> RR ()
transTopDef (FnDef _ _ _ b) = do
  transBlock b


transArg :: Arg -> RR ()
transArg x = case x of
  ArgVal type_ ident -> return ()
  ArgVar type_ ident -> return ()


transBlock :: Block -> RR ()

transBlock (Block _ []) = return ()

transBlock (Block [] (s:ss)) = do
  transStmt s
  transBlock (Block [] ss)

transBlock (Block ((VarDecl t (NoInit (Ident x))):ds) ss) = do
  l <- alloc
  local (M.insert x l) (transBlock (Block ds ss))

transBlock (Block ((VarDecl t (Init (Ident x) e)):ds) ss) = do
  l <- alloc
  v <- transExpr e
  modify (M.insert l v)
  local (M.insert x l) (transBlock (Block ds ss))


transStmt :: Stmt -> RR ()

transStmt Empty = return ()

transStmt (BStmt block) = do transBlock block

transStmt (Ass (Ident ident) expr) = do
  env   <- ask
  value <- transExpr expr
  let loc = fromMaybe (error "undefined variable") (M.lookup ident env)
  modify (M.insert loc value)

transStmt (CondElse expr stmt1 stmt2) = do
  value <- transExpr expr
  if value /= 0
    then transStmt stmt1
    else transStmt stmt2

transStmt (While expr stmt) = do
  value <- transExpr expr
  if value == 0
    then return ()
    else do
      transStmt stmt
      transStmt (While expr stmt)

transStmt x = case x of
  -- Empty -> return ()
  -- BStmt block -> return ()
  -- Ass ident expr -> return ()
  Incr ident -> return ()
  Decr ident -> return ()
  ArrayAss ident expr1 expr2 -> return ()
  DictAss ident expr1 expr2 -> return ()
  Ret expr -> return ()
  VRet -> return ()
  Cond expr stmt -> return ()
  -- CondElse expr stmt1 stmt2 -> return ()
  -- While expr stmt -> return ()
  For ident expr1 expr2 stmt -> return ()
  SExp expr -> return ()


transDecl :: Decl -> RR ()
transDecl x = case x of
  VarDecl type_ item -> return ()
  FunDecl type_ ident args block -> return ()


transItem :: Item -> RR ()
transItem x = case x of
  NoInit ident -> return ()
  Init ident expr -> return ()


transType :: Type -> RR ()
transType x = case x of
  Int -> return ()
  Str -> return ()
  Bool -> return ()
  Void -> return ()
  Array type_ -> return ()
  Dict type_1 type_2 -> return ()
  Fun type_ types -> return ()


transExpr :: Expr -> RR Integer

transExpr (EVar (Ident ident)) = do
  env   <- ask
  store <- get
  let loc = fromMaybe (error "undefined variable") (M.lookup ident env)
  return $ fromMaybe (error "unitialized variable") (M.lookup loc store)

transExpr (ELitInt integer) = return integer

transExpr (ELitTrue) = return 1

transExpr (ELitFalse) = return 0

transExpr (Neg expr) = do
  value <- transExpr expr
  return (-value)

transExpr (EMul expr1 mulop expr2) = do
  value1 <- transExpr expr1
  value2 <- transExpr expr2
  let mulop' = case mulop of Times -> (*)
                             Div   -> div
                             Mod   -> mod
  return (mulop' value1 value2)

transExpr (EAdd expr1 addop expr2) = do
  value1 <- transExpr expr1
  value2 <- transExpr expr2
  let addop' = case addop of Plus  -> (+)
                             Minus -> (-)
  return (addop' value1 value2)

transExpr x = case x of
  -- EVar ident -> return 0
  -- ELitInt integer -> return 0
  -- ELitTrue -> return 0
  -- ELitFalse -> return 0
  EApp ident exprs -> return 0
  EString string -> return 0
  ENewArray expr -> return 0
  EValArray ident expr -> return 0
  ENewDict -> return 0
  EValDict ident expr -> return 0
  -- Neg expr -> return 0
  Not expr -> return 0
  -- EMul expr1 mulop expr2 -> return 0
  -- EAdd expr1 addop expr2 -> return 0
  ERel expr1 relop expr2 -> return 0
  EAnd expr1 expr2 -> return 0
  EOr expr1 expr2 -> return 0


transAddOp :: AddOp -> RR ()
transAddOp x = case x of
  Plus -> return ()
  Minus -> return ()


transMulOp :: MulOp -> RR ()
transMulOp x = case x of
  Times -> return ()
  Div -> return ()
  Mod -> return ()


transRelOp :: RelOp -> RR ()
transRelOp x = case x of
  LTH -> return ()
  LE -> return ()
  GTH -> return ()
  GE -> return ()
  EQU -> return ()
  NE -> return ()


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
