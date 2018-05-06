f y = let
  x = 5
  in
    x + y

l :: [a] -> Int
l [] = 0
l (x:xs) = 1 + l xs

len :: [a] -> Int
len list = case list of
  []     -> 0
  (x:xs) -> (1 + len xs)

silnia n = if n == 0 then 1 else n * silnia (n-1)

headList :: [a] -> a
headList []     = error "Empty list doesn't have a head"
headList (x:xs) = x

tailList :: [a] -> [a]
tailList []     = error "Empty list doesn't have a tail"
tailList (x:xs) = xs

(-+-) :: [a] -> [a] -> [a]
[]     -+- ys = ys
(x:xs) -+- ys = x : (xs -+- ys)

takeList :: Int -> [a] -> [a]
takeList n []     = []
takeList 0 (x:xs) = []
takeList n (x:xs)
  | n > 0     = x : takeList (n-1) xs
  | otherwise = []

dropList :: Int -> [a] -> [a]
dropList _ []     = []
dropList 0 (x:xs) = (x:xs)
dropList n (_:xs)
  | n > 0     = dropList (n-1) xs
  | otherwise = []

inits :: [a] -> [[a]]
inits []     = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

inits0 xs = [take i xs | (_,i) <- zip xs [0..]]

partitions :: [a] -> [([a], [a])]
partitions []     = [([], [])]
partitions l@(x:xs) = ([], l) : [(x:ys, zs) | (ys, zs) <- partitions xs]

joinWith :: a -> [a] -> [a] -> [a]
joinWith n a b = a ++ [n] ++ b

allCasesOfInsert :: a -> [a] -> [[a]]
allCasesOfInsert n [] = [[n]]
allCasesOfInsert n list = map (\(a, b) -> joinWith n a b) (partitions list)

permutations' :: [a] -> [[a]]
permutations' []     = [[]]
permutations' (x:xs) = concat $ map (allCasesOfInsert x) (permutations' xs)

permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = [ys ++ [x] ++ zs | ws <- permutations xs,
                                         (ys, zs) <- partitions ws]

nubr :: (Eq a) => [a] -> [a]
nubr [] = []
nubr (x:xs)
  | elem x xs = nubr xs
  | otherwise = x: nubr xs

nub :: (Eq a) => [a] -> [a]
nub list = reverse (nubr (reverse list))
