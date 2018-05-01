import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.Maybe

data Exp = IntE Int
         | OpE Op Exp Exp
         | VarE String

data Decl = VarD String Exp   -- var x = e

data Stmt = S                 -- skip
          | AS String Exp     -- x := e
          | SeqS Stmt Stmt    -- S1; S2
          | IfS Exp Stmt Stmt -- if b then S1 else S2
          | WhileS Exp Stmt   -- while b do S
          | Block [Decl] Stmt -- begin [D] S end

type Op = Int -> Int -> Int

type Loc = Int

type Env = M.Map String Loc

type Store = M.Map Loc Int

type RR a = ReaderT Env (State Store) a

alloc :: RR Loc
alloc = do
  st <- get
  if (M.null st)
    then return 0
    else let (i, v) = M.findMax st in return (i + 1)


eval :: Exp -> RR Int

eval (IntE n) = return n

eval (OpE op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (op v1 v2)

eval (VarE x) = do
  env <- ask
  st <- get
  let l = fromMaybe (error "undefined variable") (M.lookup x env)
  return $ fromMaybe (error "undefined variable") (M.lookup l st)


interpret :: Stmt -> RR ()

interpret S = return ()

interpret (AS x e) = do
  env <- ask
  v <- eval e
  let l = fromMaybe (error "undefined variable") (M.lookup x env)
  modify (M.insert l v)

interpret (SeqS s1 s2) = do
  interpret s1
  interpret s2

interpret (IfS e s1 s2) = do
  v <- eval e
  if v /= 0
    then interpret s1
    else interpret s2

interpret (WhileS e s) = do
  v <- eval e
  if v == 0
    then return ()
    else do
      interpret s
      interpret (WhileS e s)

interpret (Block [] s) = interpret s
interpret (Block ((VarD x e):ds) s) = do
  l <- alloc
  v <- eval e
  modify (M.insert l v)
  local (M.insert x l) (interpret (Block ds s))

execStmt :: Stmt -> IO ()
execStmt s = mapM_ printPair $ M.toList $
              execState (runReaderT (interpret s) M.empty) M.empty
  where
    printPair :: (Int, Int) -> IO ()
    printPair (l, i) = do
      putStrLn $ (show l) ++ ", " ++ (show i)

main = execStmt testPrgW

testIB1 = Block [VarD "x" (IntE (-1))] (AS "y" (OpE (+) (VarE "y") (VarE "x")))
testIB2 = Block [VarD "x" (IntE 1)] (AS "y" (OpE (-) (VarE "y") (VarE "x")))
testWB = SeqS (IfS (VarE "y") testIB1 testIB2) (AS "x" (OpE (+) (VarE "x") (VarE "y")))
testW = WhileS (VarE "y") testWB

testPrgW = Block [VarD "x" (IntE 1000), VarD "y" (IntE 10)] testW
