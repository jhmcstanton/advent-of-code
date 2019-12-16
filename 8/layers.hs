import Data.Char (isSpace)
import Data.List (sortBy)
import Control.Monad

main = do
  nums <- takeWhile (not . isSpace) <$> readFile "in.txt"
  let layers = filter (\xs -> length xs > 0) . chunks (25 * 6) . fmap (\c -> read [c]) $ nums
  putStrLn . show . layerChecksum $ layers

chunks :: Int -> [a] -> [[a]]
chunks n xs = chunks' n [] xs where
  chunks' _ ys [] = [ys]
  chunks' 1 ys (x:xs) = (reverse $ x:ys) : chunks' n [] xs
  chunks' i ys (x:xs) = chunks' (i-1) (x:ys) xs

compareLayers :: [Int] -> [Int] -> Ordering
compareLayers xs ys = compare (zeroes xs) (zeroes ys) where
  zeroes = length . filter (== 0)

-- Part 1
layerChecksum :: [[Int]] -> Int
layerChecksum = check . head . sortBy compareLayers where
  countNums xs = (countx 1 xs, countx 2 xs)
  countx x = length . filter (== x)
  check = uncurry (*) . countNums
