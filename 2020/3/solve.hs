import System.Environment        (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  let trees = fmap mkTreeLine . lines $ contents
  let solver = solve (P (0, 0)) trees
  print . solver $ (M (3, 1))
  let solves = fmap (solver . M) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  print . product $ solves


newtype TreeLine = TL [Int]     deriving (Show)
newtype Move     = M (Int, Int) deriving (Show)
newtype Position = P (Int, Int) deriving (Show)

mkTreeLine :: String -> TreeLine
mkTreeLine s = TL (foldr appendTree [] zipped) where
  s' = s ++ s'
  zipped = zip s' [0..]
  appendTree ('#', x) xs = x : xs
  appendTree _        xs = xs

hit :: Position -> TreeLine -> Bool
hit (P (x, _)) (TL ts) =
  case dropWhile (<x) ts of
    []                   -> False
    (t : ts) | t == x    -> True
             | otherwise -> False

solve :: Position -> [TreeLine] -> Move -> Int
solve p ts (M (right, down)) = solver p ts where
  solver :: Position -> [TreeLine] -> Int
  solver p []                  = 0
  solver p@(P (x, y)) (t : ts) =
    n + solver (P (x + right, y + down)) (drop (down - 1) ts) where
      n = if hit p t then 1 else 0
