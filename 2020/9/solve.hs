{-# LANGUAGE BangPatterns #-}
import           Data.List                 (tails)
import           System.Environment        (getArgs)

main = do
  [preStr, fileName] <- getArgs
  contents <- readFile fileName
  let preLen = read preStr
      xs     = fmap read . lines $ contents
      part1  = solve1 preLen xs
  print   part1
  print $ solve2 part1 xs

solve1 :: Int -> [Integer] -> Integer
solve1 preLen nums = loop (reverse $ take preLen nums) (drop preLen nums) where
  isSum' x pre = isSum x (take preLen pre)
  loop :: [Integer] -> [Integer] -> Integer
  loop pre (x : xs) | isSum' x pre = loop (x : pre) xs
                    | otherwise   = x

isSum :: Integer -> [Integer] -> Bool
isSum x pre = any (==x) [l + r | l <- pre, r <- pre, l /= r]

solve2 :: Integer -> [Integer] -> Integer
solve2 p xs = maximum series + minimum series where
  series = head . filter (not . null) . fmap (sumsTo p) . tails $ xs

sumsTo :: Integer -> [Integer] -> [Integer]
sumsTo end xs = loop 0 [] xs where
  loop _ _ []       = []
  loop !s ys (x:xs) | s + x == end = x : ys
                    | s + x >  end = []
                    | s + x <  end = loop (s + x) (x : ys) xs
