module RegExtra where
import Mon
import Reg
import Data.List

data AB = A | B deriving (Eq,Ord,Show)

data SimulState c = S [c] (Reg c)

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


rearrange :: Reg c -> Reg c
rearrange (x :> y) = case x of
  (x1 :> x2) -> (rearrange (x1 :> (rearrange (x2 :> y))))
  _          -> (rearrange x) :> (rearrange y)
rearrange (x :| y) = case x of
  (x1 :| x2) -> (rearrange (x1 :| (rearrange (x2 :| y))))
  _          -> (rearrange x) :| (rearrange y)
rearrange (Many x) = Many (rearrange x)
rearrange x        = x

simplifyMany :: Reg c -> Reg c
simplifyMany (Many x) = simplifyMany (simplify x)
simplifyMany Empty    = Eps :| Empty
simplifyMany Eps      = Eps
simplifyMany x        = Many x

simplifyConcat :: Reg c -> Reg c -> Reg c
simplifyConcat x Empty = Empty
simplifyConcat Empty y = Empty
simplifyConcat x Eps   = x
simplifyConcat Eps y   = y
simplifyConcat x y     = x :> y

simplifyAlter :: Reg c -> Reg c -> Reg c
simplifyAlter Empty Empty = Empty
simplifyAlter Eps Eps     = Eps
simplifyAlter x y         = x :| y

simplify :: Reg c -> Reg c
simplify (Lit c)  = Lit c
simplify (Many x) = simplifyMany (simplify x)
simplify (x :> y) = simplifyConcat (simplify x) (simplify y)
simplify (x :| y) = simplifyAlter (simplify x) (simplify y)
simplify x        = x

simpl :: Reg c -> Reg c
simpl = rearrange . simplify


subStr :: Eq c => SimulState c -> SimulState c
subStr (S [] x)    = (S [] x)
subStr (S l Empty) = (S l Empty)
subStr (S l Eps)   = (S l Eps)
subStr (S (x:xs) (Lit c))
  | c == x    = (S xs Eps)
  | otherwise = (S (x:xs) Empty)
subStr (S l (Many a))
  | l_a == l   = (S l_a Eps)
  | otherwise  = subStr (S l_a (Many a))
  where
    (S l_a r_a) = subStr (S l (simpl a))
subStr (S l (a :> b)) = (S l_b r_b) where
  (S l_a r_a) = subStr (S l (simpl a))
  (S l_b r_b) = subStr (S l_a (simpl (r_a :> b)))
subStr (S l (a :| b)) = res_min where
  (S l_a r_a) = subStr (S l (simpl a))
  (S l_b r_b) = subStr (S l (simpl b))
  res_min = if (length l_a) < (length l_b) then (S l_a r_a) else (S l_b r_b)

der :: Eq c => c -> Reg c -> Reg c
der c = ders [c]

ders :: Eq c => [c] -> Reg c -> Reg c
ders c r
  | not_empty = Empty
  | otherwise = new_r
  where
    (S new_l new_r) = subStr (S c (simpl r))
    not_empty = new_l /= []


accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = ders_w /= Empty && nullable ders_w
  where ders_w = ders w r

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = der c r /= Empty

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
