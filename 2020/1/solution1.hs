
import System.Environment (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  let nums = fmap read . lines $ contents
  print . solve $ [[fst, snd]      | fst <- nums, snd <- nums]
  print . solve $ [[fst, snd, thd] | fst <- nums, snd <- nums, thd <- nums]

solve :: [[Int]] -> Int
solve nums = product group where
  group = head $ filter sumIs2020 nums
  sumIs2020 xs = sum xs == 2020
