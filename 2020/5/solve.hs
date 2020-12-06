{-# LANGUAGE LambdaCase #-}
import Data.List                 (sortBy)
import System.Environment        (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  let seats = sortBy sorter . fmap parse . lines $ contents
  print . solve1 $ seats
  print . solve2 $ seats

data Seat = S Int Int deriving (Eq, Show)
type ID   = Int

parse :: String -> Seat
parse s = S (parseRow (take 7 s)) (parseCol (drop 7 s)) where
  twos = fmap (2^) [0, 1.. ]
  parse' :: (Char -> Int) -> String -> Int
  parse' f s = sum $ zipWith (*) (reverse $ fmap f s) twos
  parseRow = parse' $ \case
    'F' -> 0
    'B' -> 1
  parseCol = parse' $ \case
    'L' -> 0
    'R' -> 1

solve1 :: [Seat] -> ID
solve1 = maximum . fmap seatId

solve2 :: [Seat] -> ID
solve2 seats = seatId . fst . head . dropWhile (uncurry (==)) . zip allIDs $ seats where
  allIDs = dropWhile (/= (head seats)) [S row col | row <- [0..127], col <- [0..7]]

sorter (S r1 c1) (S r2 c2) =
  case compare r1 r2 of
    EQ -> compare c1 c2
    c  -> c

seatId :: Seat -> ID
seatId (S row col) = row * 8 + col
