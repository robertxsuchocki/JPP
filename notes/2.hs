odwroc [] = []
odwroc (x:xs) = (odwroc xs) ++ [x] -- zle

odwroc1 xs = rev xs [] where
  rev [] ak = ak
  rev (x:xs) ak = rev xs (x:ak)

revappend [] ak = ak
revappend (x:xs) ak = revappend xs (x:ak)

odwroc2 xs = revappend xs []

app2 xs ys = revappend (odwroc2 xs) ys

inits1 xs = [take i xs | i <- [0..length xs]]

initsr [] = [[]]
initsr xs = xs:(initsr (init xs))

inits2 xs = reverse $ initsr xs

inits3 [] = [[]]
inits3 xs = inits3 (init xs) ++ [xs]

tails [] = [[]]
tails xs = xs : tails (tail xs)

inits4 xs = reverse [reverse y | y <- ys] where
  ys = tails (reverse xs)

inits [] = [[]]
inits (x:xs) = []:[x:ys | ys <- inits xs]

--foldl :: (b -> a -> b) -> b -> [a] -> b

--foldl          f          b0   [a0,a1,a2...an] =
--                     f .... f (f (f b0 a0) a1) a2 ... an


--foldr :: (a -> b -> b) -> b -> [a] -> b

--foldr          f          b0   [a0,a1,a2...an] =
--                    f a0 (f a1 (f a2 ... (f an b0) ...))

---------------------------------------------------------------------------

triads :: Int -> [(Int, Int, Int)]
triads n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n],
                        x^2 + y^2 == z^2, x <= y, gcd x (gcd y z) == 1]

primes :: [Int]
primes = primes' [2..] where
  primes' (x:xs) = x : filter (\y -> mod y x > 0) (primes' xs)

factorial :: Int -> Int
factorial n = fac n 1 where
  fac 0 n = n
  fac n m = fac (n-1) (m*n)

indexAc :: Int -> Char -> String -> Int
indexAc _ _ [] = -1
indexAc n c (x:xs)
  | c == x    = n
  | otherwise = indexAc (n+1) c xs

indexOf :: Char -> String -> Maybe Int
indexOf _ []        = Nothing
indexOf c' (x':xs') = if index > 0 then (Just index) else Nothing where
  index = indexAc 0 c' (x':xs')

positionsAc :: Int -> Char -> String -> [Int]
positionsAc _ _ [] = []
positionsAc n c (x:xs) = case indexAc n c (x:xs) of
  0     -> []
  index -> index : positionsAc (n+index) c (drop (index+1) (x:xs))

positions = positionsAc 0

incAll :: [[Int]] -> [[Int]]
incAll [] = []
incAll [[]] = [[]]
incAll (x:xs) = map (+1) x : incAll xs

fac n = foldl (*) 1 [1..n]

con :: [[a]] -> [a]
con = foldl (++) []

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/=x) xs)

---------------------------------------------------------------------------

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
