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
