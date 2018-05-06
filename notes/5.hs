import Control.Monad.State
import Tree

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

newtype State s a = State{runState :: s -> (a, s)}

instance Monad (State s) where
  -- return :: a -> State s a
  return a = State $ \st -> (a, st)
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  f >>= g = State $ \st -> let (v, st') = runState f st in runState (g v) st'

runState  :: State s a -> s -> (a, s)
evalState :: State s a -> s -> a
execState :: State s a -> s -> s


gets :: MonadState s m => (s -> a) -> m a
gets f = do
  st <- get
  return (f st)

modify :: MonadState s m => (s -> s) -> m ()
modify f = do
  st <- get
  put (f st)

instance MonadState s (State s) where
  get    = State $ \st -> (st, st)
  put st = State $ \_  -> ((), st)

get :: State s s
put :: s -> State s ()

--------------------------------------------------------------------------------

data Exp = IntE Int
         | OpE Op Exp Exp
         | VarE String
         | LetE String Exp Exp

data Stmt = S -- skip
          | AS String Exp -- x := e
          | SeqS Stmt Stmt -- S1; S2
          | IfS Exp Stmt Stmt -- if b then S1 else S2
          | WhileS Exp Stmt

type Op = Int -> Int -> Int

-- eval :: Exp -> Reader (M.Map String Int) Int

-- interpret :: Stmt -> State (M.Map String Int) ()

-- M.toList :: Map k a -> [(k, a)]

execStmt :: Stmt -> IO ()
execStmt s = mapM_ wypiszPare $ M.toList $ execState (interpret s) M.empty
  where
    writePair :: (String, Int) -> IO ()
    wyypiszPare (s, i) = do {putStr $ s++", "; putStrLn $ show i}
