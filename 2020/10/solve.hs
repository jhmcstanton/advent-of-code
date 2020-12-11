import           Data.List                 (sort, tails)
import qualified Data.IntMap.Strict as Map
import           System.Environment        (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  let nums   = fmap read . lines $ contents
      sorted = 0 : (sort nums ++ [maximum nums + 3])
  print . solve1 $ sorted
  print . solve2 $ sorted

solve1 :: [Int] -> Int
solve1 xs = m Map.! 1 * m Map.! 3 where
  diffs = zipWith (\x y -> y - x) xs (tail xs)
  m     = foldr (\x m -> Map.insertWith (+) x 1 m) Map.empty diffs


solve2 :: [Int] -> Integer
solve2 []     = 0
solve2 [x]    = 1
solve2 (x:xs) = sum (fmap solve2 forks) where
  forks = takeWhile filterer . tails $ xs
  filterer :: [Int] -> Bool
  filterer []     = True
  filterer (y:ys) = y < x+4
