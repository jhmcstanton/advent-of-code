import           Data.List                 (sort)
import qualified Data.IntMap.Strict as Map
import           System.Environment        (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  let xs = fmap read . lines $ contents
  print . solve1 $ xs

solve1 :: [Int] -> Int
solve1 xs = m Map.! 1 * m Map.! 3 where
  max   = maximum xs + 3
  xs'   = 0 : (sort $ max : xs)
  diffs = zipWith (\x y -> y - x) xs' (tail xs' ++ [max])
  m     = foldr (\x m -> Map.insertWith (+) x 1 m) Map.empty diffs
