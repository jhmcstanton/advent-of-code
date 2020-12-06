{-# LANGUAGE LambdaCase #-}
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import           System.Environment        (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  let answers = split contents
  print . solve1 $ answers

type Answers = [Char]

solve1 :: [Answers] -> Int
solve1 = sum . fmap (Set.size . Set.fromList)

solve2 :: [Answers] -> Int
solve2 = undefined

split :: String -> [Answers]
split = foldr join [""] . lines where
  join :: String -> [String] -> [String]
  join ""     xs  = "" : xs
  join s (x : xs) = (s ++ x) : xs
