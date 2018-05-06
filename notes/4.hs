readOneInt :: String -> Either String Int
readOneInt s =
  if all isDigit s
    then return (read s)
    else Left (s ++ " nie jest liczbą")

readInts :: String -> Either String [Int]
readInt input = ri (words input)
ri []     = return []
ri (x:xs) = do
  i <- readOneInt x
  let j = i + 1
  is <- ri xs
  return (i:is)

readInts2 :: String -> Either String [Int]
readInts2 = mapM readOneInt . words

--mapM :: Monad m => (a -> m b) -> [a] -> m [b]
--(>>=) :: Monad m => m a -> (a -> m b) -> m b

readIntsPom :: [String] -> Either String [Int]
readIntsPom [] = return []
readIntsPom (x:xs) =
  readOneInt x >>= (\i ->
    let j = i + 1 in
      readIntsPom xs >>= (\is ->
        return (i:is)))

readInts3 = readIntPom . words


sumInts :: String -> String
sumInts s = case readInts s of
  Left s -> s
  Right l -> show (sum l)


mysequence :: Monad m => [m a] -> m [a]
mysequence [] = return []
mysequence (x:xs) = do
  hd <- x
  tl <- mysequence xs
  return (hd:tl)

mymapM :: Monad m => (a -> m b) -> [a] -> m [b]
mymapM f [] = return []
mymapM f (x:xs) = do
  hd <- f x
  tl <- mymapM f xs
  return (hd:tl)

myforM :: Monad m => [a] -> (a -> m b) -> m [b]
myforM = flip mymapM

import System.Environment

main :: IO ()
main = do {x <- getArgs; printall x}
  where printall :: [String] -> IO ()
        printall [] = return ()
        printall (x:xs) = do {putStrLn x; printall xs}

-- main :: IO ()
-- main = getArgs >>= mapM_ putStrLn
-- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()


main = do
  putStrLn "Jaki jest twój ulubiony język programowania?"
  loop
  putStrLn "Świetnie, mój też"

loop = do
  answer <- getline
  unless (answer == "Haskell") again

again = do
  putStrLn "Spróbujmy jeszcze raz: jaki jest twój ulubiony język programowania?"
  loop

main = do
  args <- getArgs
  case args of
    [fp] -> withFile fp ReadMode printCount
    _ -> printUsage

printCount :: Handle -> IO ()
printCount h = do
  contents <- hGetContents h
  let (l, w, c) = counts contents
  (putStrLn . unwords . map show) [l, w, c]

counts :: String -> (Int, Int, Int)
counts s = (length $ lines s, length $ words s, length s)

printUsage :: IO ()
printUsage = do
  pname <- getProgName
  print $ "Usage: " ++ pname ++ " [file]"
