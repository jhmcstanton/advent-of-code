{-# LANGUAGE FlexibleContexts #-}
import           Data.Array.IArray
import           Data.List                 (foldl', unfoldr)
import           System.Environment        (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  let seats = parseLines . lines $ contents
  print . solve1 $ seats

data SeatType   = Floor | Seat          deriving (Eq, Show)
data SeatStatus = Empty | Occupied      deriving (Eq, Show)
data Seat       = S SeatType SeatStatus deriving (Eq, Show)
type Position   = (Int, Int)
type Ferry      = Array (Int, Int) FSeat
type FSeat      = (Int, Int, Seat)

showFerry :: Ferry -> String
showFerry f = concat . fmap printer . elems $ f where
  printer :: FSeat -> String
  printer (_, 0, s) = '\n' : printSeat s
  printer (_, _, s) = printSeat s
  printSeat :: Seat -> String
  printSeat (S Floor _)       = "."
  printSeat (S Seat Occupied) = "#"
  printSeat (S Seat Empty)    = "L"

solve1 :: Ferry -> Int
solve1 f = foldl' counter 0 $ elems (loop f) where
  loop :: Ferry -> Ferry
  loop f = if f `eq` f' then f else loop f' where
    f' = amap (applyRule f) f
  counter :: Int -> FSeat -> Int
  counter x (_, _, s) = if isOccupied s then x + 1 else x
  applyRule :: Ferry -> FSeat -> FSeat
  applyRule _ seat@(_, _, (S Floor _)) = seat
  applyRule f seat@(y, x, (S t s))     =
    case countNeighbors f seat of
      0             -> (y, x, S t Occupied)
      n | n >= 4    -> (y, x, S t Empty)
        | otherwise -> seat

countNeighbors :: Ferry -> FSeat -> Int
countNeighbors f (y, x, _) =
  length . filter isOccupied $ [getSeat y' x' |
                                y' <- [y-1..y+1],
                                x' <- [x-1..x+1],
                                (not $ x' == x && y' == y)
                                && inRange y' x' ]
  where
    getSeat y x                  = thd $ f ! (y, x)
    inRange y x                  = y >= lowY && y <= maxY
                                   && x >= lowX && x <= maxX
    ((lowY, lowX), (maxY, maxX)) = bounds f


isOccupied :: Seat -> Bool
isOccupied (S _ Occupied) = True
isOccupied _              = False

eq :: Ferry -> Ferry -> Bool
eq f1 f2 = all (\p -> thd (f1 ! p) == thd (f2 ! p)) [ (y, x) |
                                                      y <- [lowY..maxY],
                                                      x <- [lowX..maxX]]
  where
    ((lowY, lowX), (maxY, maxX)) = bounds f1

thd :: (a, b, c) -> c
thd (_, _, c) = c

parseLines :: [String] -> Ferry
parseLines s =
  array bounds [((y, x), (y, x, parse c)) | (cs, y) <- zip s [0..], (c, x) <- zip cs [0..]]
  where
    bounds = ((0, 0), (length s - 1, width))
    width  = length (head s) - 1

parse :: Char -> Seat
parse 'L' = S Seat  Empty
parse '.' = S Floor Empty
