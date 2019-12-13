import Data.Char (isSpace)

main = do
  nums <- readNums <$> readFile "in.txt"
  let p1 = solve 12 2 nums
  putStrLn . show $ p1
  putStrLn . show . solution2 $ nums

readNums :: String -> [Int]
readNums = fmap read . takeWhile (/= "") . splitOn (',') . filter (not . isSpace)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn t xs = split t xs [] where
  split :: Eq a => a -> [a] -> [a] -> [[a]]
  split t [] []  = [[]]
  split t [] end = [end]
  split t (c : cs) acc | t == c    = (reverse acc) : split t cs []
                       | otherwise = split t cs (c : acc)

set :: a -> Int -> [a] -> [a]
set x n xs = take n xs ++ x : drop (n + 1) xs

-- Part 1
type Op = [Int] -- length 4

step :: Op -> [Int] -> [Int]
step (opNum : lp : rp : target : []) all =
  let l = all !! lp
      r = all !! rp
  in
  case opNum of
    1  -> set (l + r) target all
    2  -> set (l * r) target all
step op _ = error $ "OpCode invalid" ++ show op

run :: [Int] -> [Int]
run xs = run' 0 xs where
  run' p xs =
    let op = take 4 $ drop p xs in
    if head op == 99
    then xs
    else run' (p + 4) $ step op xs

setup :: Int -> Int -> [Int] -> [Int]
setup x y = set x 1 . set y 2

solve :: Int -> Int -> [Int] -> Int
solve x y = head . run . setup x y

-- Part 2
nounVerbPairs :: [Int] -> [(Int, Int, Int)]
nounVerbPairs start = do
  noun <- [0..99]
  verb <- [0..99]
  pure (noun, verb, solve noun verb start)

solution2 :: [Int] -> Int
solution2 start = 100 * noun + verb where
  (noun, verb, _) = head . filter (\(_, _, z) -> z == 19690720) $ pairs
  pairs           = nounVerbPairs start
