subStr :: Eq c => ([c], Reg c) -> ([c], Reg c)
subStr ([], x)    = ([], x)
subStr (l, Empty) = (l, Empty)
subStr (l, Eps)   = (l, Eps)
subStr ((x:xs), (Lit c))
  | c == x    = (xs, Eps)
  | otherwise = ((x:xs), Empty)
subStr (l, (Many) a)
  | l_a == l   = (l_a, Eps)
  | otherwise  = subStr (l_a, (Many) a)
  where
    (l_a, r_a) = subStr (l, simpl a)
subStr (l, a :> b) = (l_b, r_b) where
  (l_a, r_a) = subStr (l, simpl a)
  (l_b, r_b) = subStr (l_a, simpl (r_a :> b))
subStr (a :| b, l) = res_min where
  (l_a, r_a) = subStr (l, simpl a)
  (l_b, r_b) = subStr (l, simpl b)
  res_min = if (length l_a) < (length l_b) then (l_a, r_a) else (l_b, r_b)

der :: Eq c => c -> Reg c -> Reg c
der c = ders [c]

ders :: Eq c => [c] -> Reg c -> Reg c
ders c r
  | not_empty = Empty
  | otherwise = new_r
  where
    (new_l, new_r) = subStr (c, simpl r)
    not_empty = new_l /= []

der :: Eq c => c -> Reg c -> Reg c
der c Empty = Empty
der c Eps   = Eps
der c (Lit x)
  | x == c    = Eps
  | otherwise = Empty
der c (x :> y) = (der c x) :> y
der c (x :| y) = (der c x) :| (der c y)
der c (Many x) = (Many x)

ders :: Eq c => [c] -> Reg c -> Reg c
ders []     r = r
ders (x:xs) r = ders xs (simpl (der x r))

rearrange :: Reg c -> Reg c
rearrange (x :> y) = case x of
  (x1 :> x2) -> (rearrange (x1 :> (x2 :> y)))
  _          -> (rearrange x) :> (rearrange y)
rearrange (x :| y) = case x of
  (x1 :| x2) -> (rearrange (x1 :| (x2 :| y)))
  _          -> (rearrange x) :| (rearrange y)
rearrange (Many x) = Many (rearrange x)
rearrange x        = x

sConcat :: Reg c -> Reg c -> Reg c
sConcat x Empty = Empty
sConcat Empty y = Empty
sConcat x Eps   = x
sConcat Eps y   = y
sConcat x y     = foldr1 (:>) (findAllConcats (x :> y))

sAlter :: Eq c => Reg c -> Reg c -> Reg c
sAlter Empty Empty = Empty
sAlter Eps Eps     = Eps
sAlter x y         = foldr1 (:|) (nub (findAllAlters (x :| y)))


match :: Eq c => Reg c -> [c] -> Maybe [c]
match r l
  | currValid  = Just (head l : matched)
  | nullable r = Just []
  | otherwise  = Nothing
  where
    der_x = simpl (der (head l) r)
    currValid = not ((null l) || (empty der_x) || (nullable der_x))
    matched = case match der_x (tail l) of
      Nothing -> []
      Just a  -> a


matchList :: Eq c => Reg c -> [c] -> [[c]]
matchList r [] = []
matchList r l@(x:xs)
  | acceptsX  = [x] : extMatches
  | mayStartX = extMatches
  | otherwise = []
  where
    der_x      = simpl (der x r)
    acceptsX   = nullable der_x
    mayStartX  = not $ empty $ der_x
    extMatches = map (x:) $ matchList der_x xs

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r w
  | hasMatches = Just (last matches)
  | nullable r = Just []
  | otherwise  = Nothing
  where
    hasMatches = not (null matches)
    matches = matchList r w
