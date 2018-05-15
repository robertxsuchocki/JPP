module TypeBreve where

import AbsBreve

import Control.Monad.Reader

import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe

import System.IO

type TypeEnv = M.Map String Type

type RIO a = (ReaderT TypeEnv IO) a


argsDecl :: [Arg] -> RIO TypeEnv
argsDecl [] = do
  env <- ask
  return env
argsDecl ((Arg type_ (Ident name)):args) =
  local (M.insert name type_) (argsDecl args)

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
    then return False
    else validArgs types exprs
validArgs [] [] = return True
validArgs _ _ = return False


validProgram :: Program -> RIO Bool
validProgram (Program decls) = do
  let main = (SExp (EApp (Ident "main") []))
  valid <- validBlock (Block decls [main])
  return valid


validIdent :: Ident -> Type -> RIO Bool
validIdent (Ident name) type_ = do
  env <- ask
  case M.lookup name env of
    Just t -> return (t == type_)
    _      -> return False


validBlock :: Block -> RIO Bool
validBlock (Block (d:ds) ss) = do
  env <- validDecl d
  if (M.null env)
    then do
      liftIO $ hPutStr stderr $ "Error: Typing failed in declaration " ++
        (show d) ++ "\n"
      return False
    else local (\_ -> env) (validBlock (Block ds ss))

validBlock (Block [] (s:ss)) = do
  valid <- validStmt s
  if (valid)
    then validBlock (Block [] ss)
    else do
      liftIO $ hPutStr stderr $ "Error: Typing failed in statement " ++
        (show s) ++ "\n"
      return False

validBlock (Block _ []) = return True


hasReturnClause :: [Stmt] -> RIO Bool
hasReturnClause [] = return False
hasReturnClause ((BStmt (Block _ bss)):ss) = hasReturnClause (bss ++ ss)
hasReturnClause ((While _ stmt):ss) = hasReturnClause (stmt:ss)
hasReturnClause ((For (Ident _) _ _ stmt):ss) = hasReturnClause (stmt:ss)

hasReturnClause ((CondElse expr stmt1 stmt2):ss) = do
  valid1 <- hasReturnClause [stmt1]
  valid2 <- hasReturnClause [stmt2]
  if (valid1 && valid2)
    then return True
    else hasReturnClause ss

hasReturnClause (s:ss) = case s of
  Ret _    -> return True
  VRet     -> return True
  Break    -> return False
  Continue -> return False
  _        -> hasReturnClause ss

validDecl :: Decl -> RIO TypeEnv
validDecl (VarDecl type_ (NoInit (Ident name))) = do
  env <- ask
  if type_ /= Void
    then return (M.insert name type_ env)
    else return M.empty

validDecl (VarDecl type_ (Init (Ident name) expr)) = do
  env   <- ask
  valid <- validExpr type_ expr
  if valid && type_ /= Void
    then return (M.insert name type_ env)
    else return M.empty

validDecl (FunDecl type_ (Ident name) args block) = do
  let idents = argsIdents args
  let Block _ stmts = block
  let fun = (Fun type_ (argsTypes args))

  env   <- ask
  env'  <- argsDecl args
  let env'' = (M.insert "return" type_ (M.insert name fun env'))

  valid_b <- local (\_ -> env'') (validBlock block)
  valid_r <- hasReturnClause stmts
  let valid_a = idents == (nub idents)
  let valid_v = all (/= Void) (argsTypes args)

  if (valid_b && (valid_r || type_ == Void) && valid_a && valid_v)
    then return (M.insert name fun env)
    else return M.empty


validStmt :: Stmt -> RIO Bool
validStmt Empty = return True
validStmt (BStmt block) = validBlock block
validStmt (Ret expr) = validStmt (Ass (Ident "return") expr)
validStmt (VRet) = validIdent (Ident "return") Void
validStmt (Break) = return True
validStmt (Continue) = return True
validStmt (SExp expr) = validExprsAny [Int, Bool, Str, Void] expr
validStmt (Print expr) = validExprsAny [Int, Bool, Str] expr
validStmt (PrintLn expr) = validStmt (Print expr)

validStmt (Ass (Ident name) expr) = do
  env <- ask
  case M.lookup name env of
    Just type' -> validExpr type' expr
    _          -> return False

validStmt (ListAss (Ident name) expr1 expr2) = do
  env <- ask
  case M.lookup name env of
    Just (List type_) ->
      validExprsAll [(Int, expr1), (type_, expr2)]
    _                 -> return False

validStmt (DictAss (Ident name) expr1 expr2) = do
  env <- ask
  case M.lookup name env of
    Just (Dict type_1 type_2) ->
      validExprsAll [(type_1, expr1), (type_2, expr2)]
    _                         -> return False

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

validStmt (For (Ident name) expr1 expr2 stmt) = do
  valid_e <- validExprsAll [(Int, expr1), (Int, expr2)]
  valid_s <- local (M.insert name Int) (validStmt stmt)
  return (valid_e && valid_s)

validStmt (DictDel (Ident name) expr) = do
  env <- ask
  case M.lookup name env of
    Just (Dict type_1 _) -> validExpr type_1 expr
    _                    -> return False


validExprsAll :: [(Type, Expr)] -> RIO Bool
validExprsAll [] = return True
validExprsAll ((type_, expr):pairs) = do
  valid <- validExpr type_ expr
  if not valid
    then return False
    else validExprsAll pairs

validExprsAny :: [Type] -> Expr -> RIO Bool
validExprsAny [] expr  = return False
validExprsAny (type_:types) expr = do
  valid <- validExpr type_ expr
  if valid
    then return True
    else validExprsAny types expr

validExpr :: Type -> Expr -> RIO Bool
validExpr type_ (EVar ident) = validIdent ident type_

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

validExpr Int (EMul expr1 mulop expr2) =
  validExprsAll [(Int, expr1), (Int, expr2)]

validExpr Int (EAdd expr1 addop expr2) =
  validExprsAll [(Int, expr1), (Int, expr2)]

validExpr Bool (ERel expr1 relop expr2) =
  validExprsAll [(Int, expr1), (Int, expr2)]

validExpr Bool (EAnd expr1 expr2) =
  validExprsAll [(Bool, expr1), (Bool, expr2)]

validExpr Bool (EOr expr1 expr2) =
  validExprsAll [(Bool, expr1), (Bool, expr2)]

validExpr type_ (EApp (Ident name) exprs) = do
  env <- ask
  case M.lookup name env of
    Just (Fun t args) -> do
      valid_a <- validArgs args exprs
      return (t == type_ && valid_a)
    _                 -> return False

validExpr type_ (EValList (Ident name) expr) = do
  env <- ask
  case M.lookup name env of
    Just (List t) -> do
      valid <- validExpr Int expr
      return (t == type_ && valid)
    _             -> return False

validExpr type_ (EValDict (Ident name) expr) = do
  env <- ask
  case M.lookup name env of
    Just (Dict t1 t2) -> do
      valid <- validExpr t1 expr
      return (t2 == type_ && valid)
    _                 -> return False

validExpr Int (ListLen (Ident name)) = do
  env <- ask
  case M.lookup name env of
    Just (List _) -> return True
    _             -> return False

validExpr Bool (DictHas (Ident name) expr) = do
  env <- ask
  case M.lookup name env of
    Just (Dict type_ _) -> validExpr type_ expr
    _                   -> return False

validExpr _ _ = return False
