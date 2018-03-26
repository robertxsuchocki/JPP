module RegExtra where
import Mon
import Reg
import Data.List

data AB = A | B deriving (Eq,Ord,Show)

infix 4 ===
class Equiv a where
  (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
   r1 === r2 = simpl r1 == simpl r2


instance Mon (Reg c) where
  m1 = Eps
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


findAllConcats :: Reg c -> [Reg c]
findAllConcats (x :> y) = findAllConcats x ++ findAllConcats y
findAllConcats x        = [x]

findAllAlters :: Reg c -> [Reg c]
findAllAlters (x :| y) = findAllAlters x ++ findAllAlters y
findAllAlters x        = [x]

rearrange :: Eq c => Reg c -> Reg c
rearrange r@(x :> y) = foldr1 (:>) (findAllConcats r)
rearrange r@(x :| y) = foldr1 (:|) (nub (findAllAlters r))
rearrange (Many x)   = Many (rearrange x)
rearrange x          = x

sMany :: Reg c -> Reg c
sMany (Many x) = sMany (simplify x)
sMany Empty    = Eps
sMany Eps      = Eps
sMany x        = Many x

sConcat :: Reg c -> Reg c -> Reg c
sConcat x Empty = Empty
sConcat Empty y = Empty
sConcat x Eps   = x
sConcat Eps y   = y
sConcat x y     = x :> y

sAlter :: Reg c -> Reg c -> Reg c
sAlter Empty Empty = Empty
sAlter Eps Eps     = Eps
sAlter x y         = x :| y

simplify :: Reg c -> Reg c
simplify (Many x) = sMany (simplify x)
simplify (x :> y) = sConcat (simplify x) (simplify y)
simplify (x :| y) = sAlter (simplify x) (simplify y)
simplify x        = x

simpl :: Eq c => Reg c -> Reg c
simpl = rearrange . simplify


der :: Eq c => c -> Reg c -> Reg c
der c (Lit x)
  | x == c     = Eps
  | otherwise  = Empty
der c Eps      = Empty
der c Empty    = Empty
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
ders (x:xs) r = ders xs (simpl (der x r))


accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = nullable (ders w r)

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = simpl (der c r) /= Empty

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r w = Nothing

search :: Eq c => Reg c -> [c] -> Maybe [c]
search r w = Nothing

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = []


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
