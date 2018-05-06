import Prelude hiding(Tree, Either, Left, Right)

-- 1

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Ord

instance Show a => Show (Tree a) where
   show Empty = "Empty"
   show (Node a b c) =
     "Node " ++ show a ++ " (" ++ show b ++ ") (" ++ show c ++ ")"

instance Eq a => Eq (Tree a) where
  Empty == Empty = True
  t1 == t2 = n1 == n2 && a1 == a2 && b1 == b2 where
    Node n1 (a1) (b1) = t1
    Node n2 (a2) (b2) = t2
  _ == _ = False

instance Functor Tree where
  fmap f Empty        = Empty
  fmap f (Node n a b) = Node (f n) (fmap f a) (fmap f b)

toList :: Tree a -> [a]
toList Empty        = []
toList (Node n a b) = (toList a) ++ [n] ++ (toList b)

toList' :: Tree a -> [a]
toList' t = attach t []
  where
    attach :: Tree a -> [a] -> [a]
    attach Empty list = list
    attach (Node n a b) list = attach a ([n] ++ attach b list)

insert :: (Ord a) => a -> Tree a -> Tree a
insert k Empty = Node k Empty Empty
insert k (Node n a b)
  | k < n     = Node n (insert k a) b
  | otherwise = Node n a (insert k b)

contains :: (Ord a) => a -> Tree a -> Bool
contains k Empty = False
contains k (Node n a b)
  | k == n    = True
  | k < n     = contains k a
  | otherwise = contains k b

fromList :: (Ord a) => [a] -> Tree a
fromList []     = Empty
fromList (x:xs) = insert x (fromList xs)

-- 2

data Either a b = Left a | Right b

instance Functor (Either e) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x)  = Left x

reverseRight :: Either e [a] -> Either e [a]
reverseRight (Left x)   = Left x
reverseRight (Right []) = Right []
reverseRight (Right l)  = Right (reverse l)

reverseRight' :: Either e [a] -> Either e [a]
reverseRight' = fmap (reverse)

-- 3

filterWords :: String -> [String]
filterWords input = filter (all isDigit) $ words $ input

readInt :: String -> Either String Int
readInt s = if (all isDigit s) then Right (read s) else Left ("Not a number: " ++ s)

readInts :: String -> Either String [Int]
readInts input = walk (words input)
  where walk [] = Right []
        walk (x:xs) = case readInt x of
                      Right i -> case walk xs of
                                 Right is -> Right (i:is)
                                 Left s -> Left s
                      Left s -> Left s


sumInts :: String -> String
sumInts input = case readInts input of
                Right r -> show (foldl (+) o r) 
                Left l -> l
