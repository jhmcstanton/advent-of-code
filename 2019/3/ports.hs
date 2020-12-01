{-# Language GeneralizedNewtypeDeriving #-}
import Data.List (sort)

data Segment     = Vertical Position Position | Horizontal Position Position deriving Show
newtype Position = Position { unPos :: (Int, Int) } deriving (Show, Eq)

main = do
  [w1, w2] <- mkWires <$> readFile "in.txt"
  putStrLn . show $ closest w1 w2

mkWires :: String -> [[Segment]]
mkWires = fmap (segments (Position (0,0)) . splitOn ',') . lines

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn t xs = split t xs [] where
  split :: Eq a => a -> [a] -> [a] -> [[a]]
  split t [] []  = [[]]
  split t [] end = [reverse end]
  split t (c : cs) acc | t == c    = (reverse acc) : split t cs []
                       | otherwise = split t cs (c : acc)

(.+-) :: Position -> Int -> Position
(Position (x, y)) .+- d = Position (x + d, y)

(.+|) :: Position -> Int -> Position
(Position (x, y)) .+| d = Position (x, y + d)

segments :: Position -> [String] -> [Segment]
segments _ []          = []
segments p@(Position (x, y)) (dir : rest) =
  case dir of
    ('R' : n) -> let p' = p .+- read n in
                 Horizontal p p' : segments p' rest
    ('L' : n) -> let p' = p .+- negate (read n) in
                 Horizontal p p' : segments p' rest
    ('U' : n) -> let p' = p .+| read n in
                 Vertical p p' : segments p' rest
    ('D' : n) -> let p' = p .+| negate (read n) in
                 Vertical p p' : segments p' rest
    _ -> error $ "What happened? " ++ show dir

intersections :: [Segment] -> [Segment] -> [Position]
intersections xs ys = do
  x <- xs
  y <- ys
  case intersect x y of
    Nothing -> mempty
    Just p  -> pure p

intersect :: Segment -> Segment -> Maybe Position
intersect   (Horizontal _  _)   (Horizontal _  _) = Nothing
intersect   (Vertical   _  _)   (Vertical   _  _) = Nothing
intersect v@(Vertical   _  _) h@(Horizontal _  _) = intersect h v
intersect (Horizontal (Position (hx, y)) (Position (hx', _)))
          (Vertical   (Position (x, vy)) (Position (_, vy'))) =
  if (y >= min vy vy' && y <= max vy vy') && (x >= min hx hx' && x <= max hx hx')
  then Just (Position (x, y))
  else Nothing

manhattanDistance :: Position -> Int
manhattanDistance (Position (x, y)) = (abs x) + (abs y)

-- Part 1
closest :: [Segment] -> [Segment] -> Int
closest ls rs = minimum . filter (/= 0) . fmap manhattanDistance $ intersections ls rs
