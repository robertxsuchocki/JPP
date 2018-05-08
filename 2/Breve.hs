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

data Value = VVoid | VInt Integer | VBool Bool
           | VString String | VFunc Env [Arg] Block
  deriving (Eq, Ord, Show, Read)

type Op a b = a -> a -> b

type Env = M.Map String Loc

type Loc = Integer

type Store = M.Map Loc Value

type RR a = ReaderT Env (State Store) a

alloc :: RR Loc
alloc = do
  st <- get
  if (M.null st)
    then return 0
    else let (loc, value) = M.findMax st in return (loc + 1)


transIdent :: Ident -> RR Value
transIdent (Ident ident) = do
  env   <- ask
  store <- get
  let loc = fromMaybe (error "undefined variable") (M.lookup ident env)
  return $ fromMaybe (error "unitialized variable") (M.lookup loc store)


transProgram :: Program -> RR ()
transProgram (Program (topdef:_)) = do
  transTopDef topdef


transTopDef :: TopDef -> RR ()
transTopDef (FnDef _ _ _ block) = do
  transBlock block


transArg :: [(Arg, Expr)] -> RR Env

transArg [] = do
  env <- ask
  return env

transArg (((Arg _ (Ident ident)), expr):args) = do
  env   <- ask
  loc   <- alloc
  value <- transExpr expr
  modify (M.insert loc value)
  local (M.insert ident loc) (transArg args)


transBlock :: Block -> RR ()

transBlock (Block (d:ds) ss) = do
  env <- transDecl d
  local (env) (transBlock (Block ds ss))

transBlock (Block [] (s:ss)) = do
  transStmt s
  transBlock (Block [] ss)

transBlock (Block _ []) = return ()


transStmt :: Stmt -> RR ()

transStmt Empty = return ()

transStmt (BStmt block) = do transBlock block

transStmt (Ass (Ident ident) expr) = do
  env   <- ask
  value <- transExpr expr
  let loc = fromMaybe (error "undefined variable") (M.lookup ident env)
  modify (M.insert loc value)

transStmt (Cond expr stmt) = do transStmt (CondElse expr stmt Empty)

transStmt (CondElse expr stmt1 stmt2) = do
  (VBool value) <- transExpr expr
  if value
    then transStmt stmt1
    else transStmt stmt2

transStmt (While expr stmt) = do
  let stmt' = BStmt (Block [] [stmt, (While expr stmt)])
  transStmt (CondElse expr stmt' Empty)

transStmt (SExp expr) = do
  value <- transExpr expr
  return ()

transStmt x = case x of
  -- Empty -> return ()
  -- BStmt block -> return ()
  -- Ass ident expr -> return ()
  ArrayAss ident expr1 expr2 -> return ()
  DictAss ident expr1 expr2 -> return ()
  Ret expr -> return ()
  VRet -> return ()
  -- Cond expr stmt -> return ()
  -- CondElse expr stmt1 stmt2 -> return ()
  -- While expr stmt -> return ()
  For ident expr1 expr2 stmt -> return ()
  -- SExp expr -> return ()


transDecl :: Decl -> RR (Env -> Env)

transDecl (VarDecl _ (NoInit (Ident ident))) = do
  env <- ask
  loc <- alloc
  modify (M.insert loc VVoid)
  return (M.insert ident loc)

transDecl (VarDecl _ (Init (Ident ident) expr)) = do
  env   <- ask
  loc   <- alloc
  value <- transExpr expr
  modify (M.insert loc value)
  return (M.insert ident loc)

transDecl (FunDecl _ (Ident ident) args block) = do
  env  <- ask
  loc  <- alloc
  modify (M.insert loc (VFunc (M.insert ident loc env) args block))
  return (M.insert ident loc)


transType :: Type -> RR ()
transType x = case x of
  Int -> return ()
  Str -> return ()
  Bool -> return ()
  Void -> return ()
  Array type_ -> return ()
  Dict type_1 type_2 -> return ()
  Fun type_ types -> return ()


transExpr :: Expr -> RR Value

transExpr (Incr (Ident ident)) = do
  env <- ask
  let loc = fromMaybe (error "undefined variable") (M.lookup ident env)
  (VInt value) <- transIdent (Ident ident)
  modify (M.insert loc (VInt (value + 1)))
  return (VInt value)

transExpr (Decr (Ident ident)) = do
  env <- ask
  let loc = fromMaybe (error "undefined variable") (M.lookup ident env)
  (VInt value) <- transIdent (Ident ident)
  modify (M.insert loc (VInt (value - 1)))
  return (VInt value)

transExpr (EVar ident) = do
  value <- transIdent ident
  return value

transExpr (ELitInt integer) = return (VInt integer)

transExpr (ELitTrue) = return (VBool True)

transExpr (ELitFalse) = return (VBool False)

transExpr (EApp ident exprs) = do
  (VFunc env args block) <- transIdent ident
  env' <- local (\_ -> env) (transArg (zip args exprs))
  local (\_ -> env') (transBlock block)
  return (VInt 0)

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
  return (VInt (op value1 value2))

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
  -- Incr expr -> return (VInt 0)
  -- Decr expr -> return (VInt 0)
  -- EVar ident -> return (VInt 0)
  -- ELitInt integer -> return (VInt 0)
  -- ELitTrue -> return (VInt 0)
  -- ELitFalse -> return (VInt 0)
  -- EApp ident exprs -> return (VInt 0)
  -- EString string -> return (VInt 0)
  ENewArray expr -> return (VInt 0)
  EValArray ident expr -> return (VInt 0)
  ENewDict -> return (VInt 0)
  EValDict ident expr -> return (VInt 0)
  -- Neg expr -> return (VInt 0)
  -- Not expr -> return (VInt 0)
  -- EMul expr1 mulop expr2 -> return (VInt 0)
  -- EAdd expr1 addop expr2 -> return (VInt 0)
  -- ERel expr1 relop expr2 -> return (VInt 0)
  -- EAnd expr1 expr2 -> return (VInt 0)
  -- EOr expr1 expr2 -> return (VInt 0)


transAddOp :: AddOp -> RR (Op Integer Integer)
transAddOp x = case x of
  Plus  -> return (+)
  Minus -> return (-)


transMulOp :: MulOp -> RR (Op Integer Integer)
transMulOp x = case x of
  Times -> return (*)
  Div   -> return div
  Mod   -> return mod


transRelOp :: RelOp -> RR (Op Integer Bool)
transRelOp x = case x of
  LTH -> return (<)
  LE  -> return (<=)
  GTH -> return (>)
  GE  -> return (>=)
  EQU -> return (==)
  NE  -> return (/=)


execProg :: Program -> String
execProg prog = concat $ map printPair $ M.toList $
              execState (runReaderT (transProgram prog) M.empty) M.empty
  where
    printPair :: (Integer, Value) -> String
    printPair (loc, value) = (show loc) ++ ", " ++ (show value) ++ "\n"

main = do
  interact breve
  putStrLn ""

breve code =
  let Ok prog = pProgram (myLexer code)
  in (execProg prog)
