type F = L
data E = A | CF F
type L = [E]

parseF :: [Char] -> (F, [Char])

parseE :: [Char] -> (E, [Char])
parseE ('a':cs)   = (A, cs)
parseE cs@('(':_) = let (f, cs') = parse F cs in (CF f, cs')

parseL :: [Char] -> (L, [Char])
parseL [] = ([], [])
parseL (')':l) = ([], '(':l)
parseL (c:l) | c == 'a' || c == 'l' = let (e, s1) = parseE (c:l)
                                      in let (l1, s2) = parse s1
                                      in (e:l1, s2)

parseF s = let s1 = zjedz '(' s
           in let (l, s2) = parseL s1
           in (l, zjedz ')' s2)

zjedz :: Char -> [Char] -> [Char]
zjedz c (dl)  | c = d     -> l
              | otherwise -> error "ZUO"
zjedz _ [] = error "ZUO"
