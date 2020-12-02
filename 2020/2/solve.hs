
import Prelude            hiding (splitAt)
import System.Environment        (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  print . solve correct1 $ contents
  print . solve correct2 $ contents

data Requirement = R {
    low :: Int, high :: Int, char :: Char
  } deriving (Show)

newtype Password = P String deriving (Show)

splitAt :: (a -> Bool) -> [a] -> ([a], [a])
splitAt f xs = (takeWhile f xs, tail $ dropWhile f xs)

parse :: String -> (Requirement, Password)
parse s        = (req, pass) where
  (rstr, pstr) = splitAt (/= ':') s
  pass         = P . tail $ pstr
  (nstr, [c])    = splitAt (/= ' ') rstr
  (lstr, hstr) = splitAt (/= '-') nstr
  req          = R (read lstr) (read hstr) c

correct1 :: (Requirement, Password) -> Bool
correct1 ((R low high c), (P pass)) = numCs >= low && numCs <= high
  where
    numCs = foldr (\c' n -> if c' == c then n+1 else n) 0 pass

correct2 :: (Requirement, Password) -> Bool
correct2 ((R low high c), (P pass)) = lc /= hc && (lc == c || hc == c)
  where
    lc = pass !! (low - 1)
    hc = pass !! (high - 1)


solve :: ((Requirement, Password) -> Bool) -> String -> Int
solve correct = length . filter id . fmap (correct . parse) . lines
