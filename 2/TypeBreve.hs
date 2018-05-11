module TypeBreve where

import AbsBreve

import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.Maybe

type TypeEnv = M.Map String Type

type RIO a = (ReaderT TypeEnv IO) a


validProgram :: Program -> RIO Bool
validProgram (Program (topdef:_)) = do
  valid <- validTopDef topdef
  return valid


validTopDef :: TopDef -> RIO Bool
validTopDef (FnDef _ _ _ block) = do
  valid <- validBlock block
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
      liftIO $ print $ "Typing failed in declaration: " ++ (show d)
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
      liftIO $ print $ "Typing failed in statement: " ++ (show s)
      return False

validBlock (Block _ []) = return True


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
  env   <- ask
  env'  <- argsDecl args
  let fun = (Fun type_ (argsToTypes args))
  let env'' = (M.insert "return" type_ (M.insert ident fun env'))
  valid <- local (\_ -> env'') (validBlock block)
  if valid
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


argsToTypes :: [Arg] -> [Type]

argsToTypes [] = []
argsToTypes ((Arg type_ _):args) = type_ : (argsToTypes args)


validArgs :: [Type] -> [Expr] -> RIO Bool

validArgs [] [] = do
  return True

validArgs (type_:types) (expr:exprs) = do
  valid <- validExpr type_ expr
  if (not valid)
    then do
      return False
    else do
      valid' <- validArgs types exprs
      return valid'

validArgs _ _ = do
  return False


validStmt :: Stmt -> RIO Bool

validStmt Empty = do
  return True

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

validStmt (Ret expr) = do
  valid <- validStmt (Ass (Ident "return") expr)
  return valid

validStmt (VRet) = do
  valid <- validIdent (Ident "return") Void
  return valid

validStmt (Print expr) = do
  valid_i <- validExpr Int expr
  valid_b <- validExpr Bool expr
  valid_s <- validExpr Str expr
  valid_v <- validExpr Void expr
  return (valid_i || valid_b || valid_s || valid_v)

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
  valid_s  <- validStmt stmt
  return (valid_e1 && valid_e2 && valid_s)

validStmt (SExp expr) = do
  valid_i <- validExpr Int expr
  valid_b <- validExpr Bool expr
  valid_s <- validExpr Str expr
  valid_v <- validExpr Void expr
  return (valid_i || valid_b || valid_s || valid_v)


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


validExpr Int (ELitInt integer) = do
  return True

validExpr Int (Incr ident) = do
  valid <- validIdent ident Int
  return valid

validExpr Int (Decr ident) = do
  valid <- validIdent ident Int
  return valid

validExpr Int (EStrInt expr) = do
  valid <- validExpr Str expr
  return valid

validExpr Int (Neg expr) = do
  valid <- validExpr Int expr
  return valid

validExpr Int (EMul expr1 mulop expr2) = do
  valid1 <- validExpr Int expr1
  valid2 <- validExpr Int expr2
  return (valid1 && valid2)

validExpr Int (EAdd expr1 addop expr2) = do
  valid1 <- validExpr Int expr1
  valid2 <- validExpr Int expr2
  return (valid1 && valid2)


validExpr Bool (ELitTrue) = do
  return True

validExpr Bool (ELitFalse) = do
  return True

validExpr Bool (Not expr) = do
  valid <- validExpr Bool expr
  return valid

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


validExpr Str (EString string) = do
  return True

validExpr Str (EIntStr expr) = do
  valid <- validExpr Int expr
  return valid


validExpr type_ expr = do
  return False
