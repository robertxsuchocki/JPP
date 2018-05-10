module TypeBreve where

import AbsBreve

import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.Maybe

type Op a b = a -> a -> b

type Env = M.Map String Type

type RR a = ReaderT Env IO a


validIdent :: Ident -> Type -> RR Bool
validIdent (Ident ident) type_ = do
  env <- ask
  let type_ = M.lookup ident env
  case type_ of Nothing  -> return False
                (Just t) -> return t == type_


validProgram :: Program -> RR Bool
validProgram (Program (topdef:_)) = do
  validTopDef topdef


validTopDef :: TopDef -> RR Bool
validTopDef (FnDef _ _ _ block) = do
  validBlock block


-- validArg :: [(Arg, Value)] -> RR Env
--
-- validArg [] = do
--   env <- ask
--   return env
--
-- validArg (((Arg _ (Ident ident)), value):args) = do
--   env   <- ask
--   local (M.insert ident type_) (validArg args)


-- validBlock :: Block -> RR ()
--
-- validBlock (Block (d:ds) ss) = do
--   env <- validDecl d
--   local (env) (validBlock (Block ds ss))
--
-- validBlock (Block [] (s:ss)) = do
--   validStmt s
--   returned <- validReturnStatus
--   if (not returned)
--     then validBlock (Block [] ss)
--     else return ()
--
-- validBlock (Block _ []) = return ()


validStmt :: Stmt -> RR ()

validStmt Empty = True

validStmt (BStmt block) = do
  validBlock block

validStmt (Ass (Ident ident) expr) = do
  env <- ask
  let type_ = M.lookup ident env
  case type_ of Nothing     -> return False
                (Just Int)  -> return transInt expr
                (Just Bool) -> return transBool expr
                (Just Str)  -> return transStr expr

validStmt (Ret expr) = do
  validStmt (Ass (Ident "return") expr)

validStmt (VRet) = do
  validIdent (Ident "return") Void

validStmt (Print expr) = do
  return True

validStmt (Cond expr stmt) = do
  validStmt (CondElse expr stmt Empty)

validStmt (CondElse expr stmt1 stmt2) = do
  validBool expr
  validStmt stmt1
  validStmt stmt2

validStmt (While expr stmt) = do
  validBool expr
  validStmt stmt

validStmt (For (Ident ident) expr1 expr2 stmt) = do
  validInt expr1
  validInt expr2
  validStmt stmt

validStmt (SExp expr) = do
  validExpr expr

validStmt x = case x of
  ArrayAss ident expr1 expr2 -> return ()
  DictAss ident expr1 expr2 -> return ()


validDecl :: Decl -> RR Bool

validDecl (VarDecl type_ (NoInit (Ident ident))) = do
  return True

validDecl (VarDecl type_ (Init (Ident ident) expr)) = do
  case type_ of Int  -> validInt expr
                Bool -> validBool expr
                Str  -> validStr expr
                _    -> return False

validDecl (FunDecl type_ (Ident ident) args block) = do
  validArgs args


validType :: Type -> RR ()
validType x = case x of
  Int -> return ()
  Str -> return ()
  Bool -> return ()
  Void -> return ()
  Array type_ -> return ()
  Dict type_1 type_2 -> return ()
  Fun type_ types -> return ()


-- validExpr :: Expr -> RR Bool
--
-- validExpr (EApp ident exprs) = do
--   values <- mapM validExpr exprs
--   (VFunc env args block) <- validIdent ident VFunc
--
--   env'   <- local (\_ -> env) (validArg (zip args values))
--   local (\_ -> env') (validBlock block)
--
--   let type_ = fromMaybe 0 (M.lookup "return" env)
--   modify (M.insert type_ VNothing)
--   return value


validInt :: Expr -> RR Bool

validInt (ELitInt integer) = do
  return True

validInt (EVar ident) = do
  validIdent ident Int

validInt (Incr (Ident ident)) = do
  validIdent ident Int

validInt (Decr (Ident ident)) = do
  validIdent ident Int

validInt (EStrInt expr) = do
  validStr expr

validInt (Neg expr) = do
  validInt expr

validInt (EMul expr1 mulop expr2) = do
  validInt expr1
  validInt expr2

validInt (EAdd expr1 addop expr2) = do
  validInt expr1
  validInt expr2

validInt _ = do
  return False


validBool :: Expr -> RR Bool

validBool (ELitTrue) = do
  return True

validBool (ELitFalse) = do
  return True

validBool (EVar ident) = do
  validIdent ident Bool

validBool (Not expr) = do
  validBool expr

validBool (ERel expr1 relop expr2) = do
  validInt expr1
  validInt expr2

validBool (EAnd expr1 expr2) = do
  validBool expr1
  validBool expr2

validBool (EOr expr1 expr2) = do
  validBool expr1
  validBool expr2

validBool _ = do
  return False


validStr :: Expr -> RR Bool

validStr (EString string) = do
  return True

validStr (EVar ident) = do
  validIdent ident Str

validStr (EIntStr expr) = do
  validInt expr

validStr _ = do
  return False


execProg :: Program -> IO ()
execProg prog = do -- void (execStateT (runReaderT (validProgram prog) M.empty) M.empty)
  state <- execStateT (runReaderT (validProgram prog) M.empty) M.empty
  printPairs $ M.toList $ state
    where
      printPairs []     = putStr ""
      printPairs (p:ps) = do
        putStrLn (show p)
        printPairs ps

main :: IO ()
main = do
  code <- getContents
  let Ok prog = pProgram (myLexer code)
  execProg prog
