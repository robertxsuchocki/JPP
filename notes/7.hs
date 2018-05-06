ren :: Tree a -> (Reader Int (Tree Int))
ren Empty = return Empty
ren (Node x s t) = do
  i <- ask
  s' <- local (+1) (ren s)
  t' <- local (+1) (ren t)
  return $ Node i s' t'

renumber = runReader (ren t) 0

--------------------------------------------------------------------------------

data Exp = EInt Int
         | EOp Op Exp Exp
         | EVar String
         | ELet String Exp Exp

data Op = OpAdd | OpMul | OpSub

type Interpreter = Reader (M.Map Var Int)

evalOp :: Op -> Interpreter (Int -> Int -> Int)
evalOp OpAdd = return (+)
evalOp OpMul = return (*)
evalOp OpSub = return (-)

evalExp :: Exp -> Interpreter Int
evalExp (EInt x) = return n
evalExp (EOp op e1 e2) = do
  x1 <- evalExp e1
  x2 <- evalExp e2
  opf <- evalOp op
  return (opf x1 x2)

evalExp (EVar x) = do
  env <- ask
  return $ fromMaybe (error "undefined variable") (M.lookup x env)

evalExp (ELet x e1 e2) = do
  v <- evalExp e1
  local (M.insert x v) (evalExp e2)

eval :: Exp -> Int
eval e = runReader Interpreter 0

test = ELet "y" (ELet "y" (EOp OpAdd (EInt 6) (EInt 9))
                      (EOp OpSub y (EInt 1)))
                (EOp OpMul y (EInt 3))
  where x = EVar "x"
        y = EVar "y"

--------------------------------------------------------------------------------

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

alloc :: Store -> Loc
alloc m = if (M.null m) then 0
          else let (i, w) = M.findMax m in i+1

alloc' :: RR Loc
alloc' = do
  m <- get
  if (M.null m) then return 0
  else let (i, w) = M.findMax m in return (i+1)

eval :: Exp -> SS Int

eval (IntE n) = return n

eval (OpE op e1 e2) = do
  x1 <- eval e1
  x2 <- eval e2
  return (op x1 x2)

eval (VarE x) = do
env <- ask
st <- get
let l = fromMaybe (error "undefined variable") (M.lookup x env)
return $ fromMaybe (error "undefined variable") (M.lookup l st)

eval (LetE x e1 e2) = do
v <- eval e1
l <- alloc'
modify (M.insert l v)
local (M.insert x l) e2

interpret :: Stmt -> RR ()
interpret S = return ()

-- TODO

execStmt s = mapM_ wypiszPare $ M.toList $
              execState (runReaderT (interpret s) M.empty) M.empty

main = print $ evalState (runReaderT (eval testE) M.empty) M.empty

testE
