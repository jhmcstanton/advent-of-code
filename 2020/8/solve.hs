{-# LANGUAGE DeriveFunctor #-}
import           System.Environment        (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  let instructions = fmap parse . lines $ contents
      result       = run instructions
  print . fmap (\(s, _, _) -> s) $ result
  let allRuns = fmap run . allVersions $ instructions
  print . head . fmap (fmap (\(s, _, _) -> s)) .  dropWhile isLooping $ allRuns

data Instruction =
  N Int | J Int | A Int deriving (Show)

type Visited = Int

data Result a =
  Terminated a | Loop a deriving (Show, Functor)

isLooping :: Result a -> Bool
isLooping (Loop _)       = True
isLooping (Terminated _) = False

allVersions :: [Instruction] -> [[Instruction]]
allVersions is = do
  (_, i) <- filter filterer . zip is $ [0..]
  pure $ swapAt i is
  where
    filterer (N _, _) = False
    filterer _        = True
    swapAt n xs = take n xs ++ ys where
      ys' = drop n xs
      ys = (swap . head $ ys') : tail ys'

swap :: Instruction -> Instruction
swap (J i) = N i
swap (N i) = A i
swap   n   = n

run :: [Instruction] -> Result (Int, [(Instruction, Int, Visited)], [Int])
run is = loop 0 0 [] [] (zip3 is [0..] $ repeat (-1)) where
  loop acc _ pdex prev []                           = Terminated (acc, prev, pdex)
  loop acc pc pdex prev next@((i, dex, found) : js)
    | found >= 0 = Loop (acc, prev ++ next, pdex)
    | otherwise  =
    case i of
      N n             -> loop  acc      pc' dex' prev' js
      A n             -> loop (acc + n) pc' pdex prev' js
      J n | n > 0     ->
            loop acc pc' dex' (rtake (n-1) js ++ prev') (drop (n-1) js)
          | otherwise ->
            let n' = abs n in
            loop acc pc' dex' (drop n' prev) (rtake n' prev ++ (i, dex, pc) : js)
    where
      prev'   = (i, dex, pc) : prev
      rtake n = reverse . take n
      pc'     = pc + 1
      dex'    = dex : pdex

parse :: String -> Instruction
parse s = helper (words s) where
  helper ("nop" : [x]) = N $ parseInt x
  helper ("jmp" : [x]) = J $ parseInt x
  helper ("acc" : [x]) = A $ parseInt x
  parseInt ('+' : s)   = read s
  parseInt s           = read s
