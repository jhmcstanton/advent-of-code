import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import           System.Environment        (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  let answers = split contents
  print . solve1 $ answers
  print . solve2 $ answers

type Answers = [[Char]]

solve1 :: [Answers] -> Int
solve1 = sum . fmap (Set.size . Set.fromList . concat)

solve2 :: [Answers] -> Int
solve2 = sum . fmap allAnsweredCount where
  allAnsweredCount :: Answers -> Int
  allAnsweredCount a = Map.size . Map.filter (==groupSize) $ summed where
    groupSize = length a
    summed = foldr (\c m -> Map.insertWith (+) c 1 m) Map.empty $ concat a

split :: String -> [Answers]
split = foldr join [[]] . lines where
  join :: String -> [[String]] -> [[String]]
  join ""     xs  = [] : xs
  join s (x : xs) = (s : x) : xs
