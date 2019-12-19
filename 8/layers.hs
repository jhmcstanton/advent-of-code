import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.List (any, sortBy)
import Control.Monad

main = do
  nums <- takeWhile (not . isSpace) <$> readFile "in.txt"
  let layers = filter (\xs -> length xs > 0) . chunks (25 * 6) . fmap (\c -> read [c]) $ nums
  putStrLn . show . layerChecksum $ layers
  putStrLn . toStr . visibleLayer $ layers

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

-- Part 2
visibleLayer :: [[Int]] -> [Int]
visibleLayer (x:xs) = top x xs where
  top layer []     = layer
  top layer (x:xs) | any (== 0) layer = top (zipWith (\l x -> if l == 2 then x else l) layer x) xs
                   | otherwise        = layer

lineToStr :: [Int] -> String
lineToStr []       = []
lineToStr (0 : xs) = ' ' : lineToStr xs
lineToStr (1 : xs) = 'X' : lineToStr xs
lineToStr (2 : xs) = ' ' : lineToStr xs

toStr :: [Int] -> String
toStr = fold . fmap (\l -> lineToStr l ++ "\n") . chunks 25
