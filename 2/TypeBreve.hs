module TypeBreve where

import AbsBreve

import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Data.List

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.Maybe

import System.IO

type TypeEnv = M.Map String Type

type RIO a = (ReaderT TypeEnv IO) a


validProgram :: Program -> RIO Bool
validProgram (Program decls) = do
  let main = (SExp (EApp (Ident "main") []))
  valid <- validBlock (Block decls [main])
  return valid


validIdent :: Ident -> Type -> RIO Bool
validIdent (Ident ident) type_ = do
  env <- ask
  case M.lookup ident env of
    Just t -> do
      return (t == type_)
    _ -> do
      return False


validBlock :: Block -> RIO Bool

validBlock (Block (d:ds) ss) = do
  env <- validDecl d
  if (M.null env)
    then do
      liftIO $ hPutStr stderr $ "Error: Typing failed in declaration " ++ (show d) ++ "\n"
      return False
    else do
      valid <- local (\_ -> env) (validBlock (Block ds ss))
      return valid

validBlock (Block [] (s:ss)) = do
  valid <- validStmt s
  if (valid)
    then do
      valid <- validBlock (Block [] ss)
      return valid
    else do
      liftIO $ hPutStr stderr $ "Error: Typing failed in statement " ++ (show s) ++ "\n"
      return False

validBlock (Block _ []) = return True


hasReturnClause :: [Stmt] -> RIO Bool

hasReturnClause [] = return False

hasReturnClause ((BStmt (Block ds bss)):ss) =
  hasReturnClause (bss ++ ss)

hasReturnClause ((CondElse expr stmt1 stmt2):ss) = do
  valid1 <- hasReturnClause [stmt1]
  valid2 <- hasReturnClause [stmt2]
  if (valid1 && valid2)
    then return True
    else hasReturnClause ss

hasReturnClause ((While expr stmt):ss) =
  hasReturnClause (stmt:ss)

hasReturnClause ((For (Ident ident) expr1 expr2 stmt):ss) =
  hasReturnClause (stmt:ss)

hasReturnClause (s:ss) = case s of
  Ret _    -> return True
  VRet     -> return True
  Break    -> return False
  Continue -> return False
  _        -> hasReturnClause ss


validDecl :: Decl -> RIO TypeEnv

validDecl (VarDecl type_ (NoInit (Ident ident))) = do
  env <- ask
  return (M.insert ident type_ env)

validDecl (VarDecl type_ (Init (Ident ident) expr)) = do
  env   <- ask
  valid <- validExpr type_ expr
  if valid
    then do
      return (M.insert ident type_ env)
    else do
      return M.empty

validDecl (FunDecl type_ (Ident ident) args block) = do
  let idents = argsIdents args
  let Block _ stmts = block
  let fun = (Fun type_ (argsTypes args))

  env   <- ask
  env'  <- argsDecl args
  let env'' = (M.insert "return" type_ (M.insert ident fun env'))

  valid_b <- local (\_ -> env'') (validBlock block)
  let valid_a = idents == (nub idents)
  valid_r <- hasReturnClause stmts

  if (valid_b && valid_a && (valid_r || type_ == Void))
    then do
      return (M.insert ident fun env)
    else do
      return M.empty


argsDecl :: [Arg] -> RIO TypeEnv
argsDecl [] = do
  env <- ask
  return env
argsDecl ((Arg type_ (Ident ident)):args) = do
  local (M.insert ident type_) (argsDecl args)


argsTypes :: [Arg] -> [Type]
argsTypes [] = []
argsTypes ((Arg type_ _):args) = type_ : (argsTypes args)


argsIdents :: [Arg] -> [Ident]
argsIdents [] = []
argsIdents ((Arg _ ident):args) = ident : (argsIdents args)


validArgs :: [Type] -> [Expr] -> RIO Bool
validArgs (type_:types) (expr:exprs) = do
  valid <- validExpr type_ expr
  if (not valid)
    then do
      return False
    else do
      valid' <- validArgs types exprs
      return valid'
validArgs [] [] = return True
validArgs _ _ = return False


validStmt :: Stmt -> RIO Bool

validStmt Empty = return True

validStmt (BStmt block) = do
  valid <- validBlock block
  return valid

validStmt (Ass (Ident ident) expr) = do
  env <- ask
  case M.lookup ident env of
    Just type' -> do
      valid <- validExpr type' expr
      return valid
    _ -> do
      return False

validStmt (ListAss (Ident ident) expr1 expr2) = do
  env <- ask
  case M.lookup ident env of
    Just (List type_) -> do
      valid1 <- validExpr Int expr1
      valid2 <- validExpr type_ expr2
      return $ valid1 && valid2
    _ -> do
      return False

validStmt (DictAss (Ident ident) expr1 expr2) = do
  env <- ask
  case M.lookup ident env of
    Just (Dict type_1 type_2) -> do
      valid1 <- validExpr type_1 expr1
      valid2 <- validExpr type_2 expr2
      return $ valid1 && valid2
    _ -> do
      return False

validStmt (Ret expr) = validStmt (Ass (Ident "return") expr)

validStmt (VRet) = validIdent (Ident "return") Void

validStmt (Break) = return True

validStmt (Continue) = return True

validStmt (Cond expr stmt) = do
  valid_e <- validExpr Bool expr
  valid_s <- validStmt stmt
  return (valid_e && valid_s)

validStmt (CondElse expr stmt1 stmt2) = do
  valid_e  <- validExpr Bool expr
  valid_s1 <- validStmt stmt1
  valid_s2 <- validStmt stmt2
  return (valid_e && valid_s1 && valid_s2)

validStmt (While expr stmt) = do
  valid_e <- validExpr Bool expr
  valid_s <- validStmt stmt
  return (valid_e && valid_s)

validStmt (For (Ident ident) expr1 expr2 stmt) = do
  valid_e1 <- validExpr Int expr1
  valid_e2 <- validExpr Int expr2
  valid_s  <- local (M.insert ident Int) (validStmt stmt)
  return (valid_e1 && valid_e2 && valid_s)

validStmt (SExp expr) = do
  valid_i <- validExpr Int expr
  valid_b <- validExpr Bool expr
  valid_s <- validExpr Str expr
  valid_v <- validExpr Void expr
  return (valid_i || valid_b || valid_s || valid_v)

validStmt (Print expr) = do
  valid_i <- validExpr Int expr
  valid_b <- validExpr Bool expr
  valid_s <- validExpr Str expr
  return (valid_i || valid_b || valid_s)

validStmt (PrintLn expr) = validStmt (Print expr)

validStmt (DictDel (Ident ident) expr) = do
  env <- ask
  case M.lookup ident env of
    Just (Dict type_1 _) -> validExpr type_1 expr
    _                    -> return False


validExpr :: Type -> Expr -> RIO Bool

validExpr type_ (EVar ident) = do
  valid <- validIdent ident type_
  return valid

validExpr type_ (EApp (Ident ident) exprs) = do
  env <- ask
  case M.lookup ident env of
    Just (Fun t args) -> do
      valid_a <- validArgs args exprs
      return (t == type_ && valid_a)
    _ -> do
      return False

validExpr type_ (EValList (Ident ident) expr) = do
  env <- ask
  case M.lookup ident env of
    Just (List t) -> do
      valid <- validExpr Int expr
      return (t == type_ && valid)
    _ -> do
      return False

validExpr type_ (EValDict (Ident ident) expr) = do
  env <- ask
  case M.lookup ident env of
    Just (Dict t1 t2) -> do
      valid <- validExpr t1 expr
      return (t2 == type_ && valid)
    _ -> do
      return False

validExpr Int (Incr ident) = validIdent ident Int
validExpr Int (Decr ident) = validIdent ident Int
validExpr Int (EStrInt expr) = validExpr Str expr
validExpr Int (ELitInt integer) = return True
validExpr Int (Neg expr) = validExpr Int expr

validExpr Bool (ELitTrue) = return True
validExpr Bool (ELitFalse) = return True
validExpr Bool (Not expr) = validExpr Bool expr

validExpr Str (EIntStr expr) = validExpr Int expr
validExpr Str (EStr string) = return True

validExpr (List _) (ENewList) = return True
validExpr (Dict _ _) (ENewDict) = return True

validExpr Int (ListLen (Ident ident)) = do
  env <- ask
  case M.lookup ident env of
    Just (List _) -> return True
    _             -> return False

validExpr Int (EMul expr1 mulop expr2) = do
  valid1 <- validExpr Int expr1
  valid2 <- validExpr Int expr2
  return (valid1 && valid2)

validExpr Int (EAdd expr1 addop expr2) = do
  valid1 <- validExpr Int expr1
  valid2 <- validExpr Int expr2
  return (valid1 && valid2)

validExpr Bool (DictHas (Ident ident) expr) = do
  env <- ask
  case M.lookup ident env of
    Just (Dict type_ _) -> validExpr type_ expr
    _                   -> return False

validExpr Bool (ERel expr1 relop expr2) = do
  valid1 <- validExpr Int expr1
  valid2 <- validExpr Int expr2
  return (valid1 && valid2)

validExpr Bool (EAnd expr1 expr2) = do
  valid1 <- validExpr Bool expr1
  valid2 <- validExpr Bool expr2
  return (valid1 && valid2)

validExpr Bool (EOr expr1 expr2) = do
  valid1 <- validExpr Bool expr1
  valid2 <- validExpr Bool expr2
  return (valid1 && valid2)

validExpr type_ expr = return False
