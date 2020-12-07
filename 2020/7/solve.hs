import           Data.Char                 (isDigit)
import           Data.List                 (intersperse)
import qualified Data.Map.Strict    as Map
import           Data.Map.Strict           (Map)
import           System.Environment        (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  let rules = Map.fromList . fmap parse . lines $ contents
      color = Color "shiny gold"
      bags  = findAll color rules
  print . Map.size $ bags
  print (internalBags color rules - 1)

newtype Color  = Color String                    deriving (Eq, Ord, Show)
type    Rule   = (Color, Map Color Int)
type    Rules  = Map Color (Map Color Int)

parse :: String -> Rule
parse s = (mc, foldr (\(n, c) m -> Map.insert c n m) Map.empty . children . drop 4 . words $ s) where
  mc = Color . concat . intersperse " " . take 2 . words $ s
  children []                              = []
  children ("no" : "other" : "bags." : []) = []
  children (num : mod : color : _ : xs)    =
    (read num, Color $ mod ++ " " ++ color) : children xs

findAll :: Color -> Rules -> Rules
findAll color rules = Map.filterWithKey (\k _ -> Map.findWithDefault False k filtered) rules where
  filtered    = looper Map.empty rules'
  rules'      = Map.toList . Map.filterWithKey (\k _ -> k /= color) $ rules
  looper :: Map Color Bool -> [Rule] -> Map Color Bool
  looper confirmed []             = confirmed
  looper confirmed ((c, cs) : rs) =
    case (Map.member color cs, all (`Map.member` confirmed) . Map.keys $ cs) of
      (False, False) -> looper confirmed (rs ++ [(c, cs)])
      (False, True ) -> if any (confirmed Map.!) . Map.keys $ cs
                        then looper (Map.insert c True confirmed) rs
                        else looper (Map.insert c False confirmed) rs
      _              -> looper (Map.insert c True confirmed) rs

-- this count includes the outer Color bag
internalBags :: Color -> Rules -> Int
internalBags color rules =
  1 + (Map.foldr (+) 0 $ Map.mapWithKey (\k v -> v * internalBags k rules) crule)
  where
    crule = rules Map.! color
