module RegExtra where
import Mon
import Reg
import Data.List

data AB = A | B deriving (Eq, Ord, Show)

infix 4 ===
class Equiv a where
  (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
   r1 === r2 = simpl r1 == simpl r2

instance Mon (Reg c) where
  m1   = Eps
  (<>) = (:>)


nullable :: Reg c -> Bool
nullable (x :> y) = nullable x && nullable y
nullable (x :| y) = nullable x || nullable y
nullable Empty    = False
nullable (Lit _)  = False
nullable _        = True

empty :: Reg c -> Bool
empty Empty = True
empty _     = False

nonempty :: Reg c -> Bool
nonempty = not . empty

gatherConcats :: Reg c -> [Reg c]
gatherConcats (x :> y) = gatherConcats x ++ gatherConcats y
gatherConcats x        = [x]

gatherAlters :: Reg c -> [Reg c]
gatherAlters (x :| y) = gatherAlters x ++ gatherAlters y
gatherAlters x        = [x]

sConcat :: Eq c => Reg c -> Reg c -> Reg c
sConcat x y
  | hasEmpty  = Empty
  | onlyEps   = Eps
  | otherwise = foldr1 (:>) withoutEps
  where
    concats    = map simpl $ gatherConcats $ x :> y
    hasEmpty   = any (== Empty) concats
    withoutEps = filter (/= Eps) concats
    onlyEps    = null withoutEps

sAlter :: Eq c => Reg c -> Reg c -> Reg c
sAlter x y = foldr1 (:|) $ map simpl $ nub $ gatherAlters $ x :| y

sMany :: Eq c => Reg c -> Reg c
sMany (Many x) = sMany x
sMany Empty    = Eps
sMany Eps      = Eps
sMany x        = Many (simpl x)

simpl :: Eq c => Reg c -> Reg c
simpl (x :> y) = sConcat x y
simpl (x :| y) = sAlter x y
simpl (Many x) = sMany x
simpl x        = x


-- https://en.wikipedia.org/wiki/Brzozowski_derivative
der :: Eq c => c -> Reg c -> Reg c
der c (Lit x)
  | x == c     = Eps
  | otherwise  = Empty
der _ Eps      = Empty
der _ Empty    = Empty
der c (Many x) = (der c x) :> (Many x)
der c (x :> y)
  | nullable x = (der_x :> y) :| der_y
  | otherwise  = der_x :> y
  where
    der_x = der c x
    der_y = der c y
der c (x :| y) = (der c x) :| (der c y)

ders :: Eq c => [c] -> Reg c -> Reg c
ders []     r = r
ders (x:xs) r = ders xs $ simpl $ der x r


accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = nullable $ ders w r

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = nonempty $ simpl $ der c r

match' :: Eq c => Reg c -> [c] -> Maybe [c]
match' _ [] = Nothing
match' r (x:xs)
  | valid      = Just (x : matched)
  | otherwise  = Nothing
  where
    der_x   = simpl $ der x r
    match_x = match' der_x xs
    valid   = nullable der_x || nonempty der_x && match_x /= Nothing
    matched = case match_x of
      Nothing -> []
      Just a  -> a

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r l = case match' r l of
  Nothing -> if nullable r then Just [] else Nothing
  x       -> x

search :: Eq c => Reg c -> [c] -> Maybe [c]
search _ [] = Nothing
search r l@(_:xs) = case match r l of
  Nothing -> search r xs
  Just a  -> Just a

cutCovered :: Int -> [[c]] -> [[c]]
cutCovered _ []     = []
cutCovered n (l:ls)
  | len_l < n = cutCovered (n - 1) ls
  | otherwise = l : cutCovered len_l ls
  where
    len_l = length l

unpackMaybes :: [Maybe a] -> [a]
unpackMaybes [] = []
unpackMaybes (x:xs) = case x of
  Nothing -> unpackMaybes xs
  Just a  -> a : unpackMaybes xs

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = cutCovered 0 $ unpackMaybes $ map (match r) $ init $ tails w


char :: Char -> Reg Char
char = Lit

string :: [Char] -> Reg Char
string = foldr1 (:>) . map Lit

alts :: [Char] -> Reg Char
alts = foldr1 (:|) . map Lit

letter = alts ['a'..'z'] :| alts ['A'..'Z']
digit = alts ['0'..'9']
number = digit :> Many digit
ident = letter :> Many (letter :| digit)

many1 r = r :> Many r
