import           System.Environment        (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  let instructions = fmap parse . lines $ contents
  -- let (s, is) = solve instructions
  -- mapM_ print is
  -- print s
  print . solve $ instructions

data Instruction =
  N Int | J Int | A Int deriving (Show)

type Visited = Bool

solve :: [Instruction] -> Int
solve is = loop 0 [] (zip is $ repeat False) where
  loop :: Int -> [(Instruction, Visited)] -> [(Instruction, Visited)] -> Int
  loop acc p n@((_, True) : _)    = acc
  loop acc prev ((i, False) : js) =
    case i of
      N n             -> loop acc prev' js
      A n             -> loop (acc + n) prev' js
      J n | n > 0     ->
            loop acc (rtake (n-1) js ++ prev') (drop (n-1) js)
          | otherwise ->
            let n' = abs n in
            loop acc (drop n' prev) (rtake n' prev ++ (i, True) : js)
    where
      prev'   = (i, True) : prev
      rtake n = reverse . take n

parse :: String -> Instruction
parse s = helper (words s) where
  helper ("nop" : [x]) = N $ parseInt x
  helper ("jmp" : [x]) = J $ parseInt x
  helper ("acc" : [x]) = A $ parseInt x
  parseInt ('+' : s)   = read s
  parseInt s           = read s
