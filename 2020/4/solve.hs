import Data.Char                 (isDigit)
import System.Environment        (getArgs)

main = do
  args     <- getArgs
  contents <- readFile (head args)
  let rawPassports = split contents
      passports    = fmap (foldr update empty . words) rawPassports
  print . length . filter valid1 $ passports
  print . length . filter valid2 $ passports

data LengthUnit = CM | IN deriving (Show, Eq)

data PassPort = P
  {
    byr :: Integer,
    iyr :: Integer,
    eyr :: Integer,
    hgt :: Integer,
    hut :: Maybe LengthUnit,
    hcl :: String,
    ecl :: String,
    pid :: String,
    cid :: Maybe Int
  } deriving (Show)

empty :: PassPort
empty = P 0 0 0 0 Nothing "" "" "" Nothing

update :: String -> PassPort -> PassPort
update s p = update' (take 3 s) (drop 4 s) where
  update' "byr" yr = p { byr = read yr                    }
  update' "iyr" yr = p { iyr = read yr                    }
  update' "eyr" yr = p { eyr = read yr                    }
  update' "hcl" hc = p { hcl = hc                         }
  update' "ecl" ec = p { ecl = ec                         }
  update' "pid" id = p { pid = id                         }
  update' "cid" cd = p { cid = Just (read cd)             }
  update' "hgt" ht = p { hgt = height, hut = unit } where
    height = read . filter isDigit $ ht
    unit   = case reverse . take 2 . reverse $ ht of
               "in" -> Just IN
               "cm" -> Just CM
               _    -> Nothing

valid1 :: PassPort -> Bool
valid1 (P byr iyr eyr hgt _ hcl ecl pid _) =
  byr * iyr * eyr * hgt /= 0
  && (not . null $ hcl)
  && (not . null $ ecl)
  && (not . null $ pid)


valid2 :: PassPort -> Bool
valid2 (P byr iyr eyr hgt hut hcl ecl pid _) =
  byr >= 1920 && byr <= 2002 &&
  iyr >= 2010 && iyr <= 2020 &&
  eyr >= 2020 && eyr <= 2030 &&
  ( (hut == Just CM && (hgt >= 150 && hgt <= 193)) ||
    (hut == Just IN && (hgt >= 59 && hgt <= 76))) &&
  validHair hcl &&
  validEye  ecl &&
  length (filter isDigit pid) == 9 where
  validHair :: String -> Bool
  validHair ('#' : s) =
    length (filter (\c -> isDigit c || c `elem` ['a'..'f']) s) == 6
  validHair _ = False
  validEye  :: String -> Bool
  validEye s = case s of
    "amb" -> True
    "blu" -> True
    "brn" -> True
    "gry" -> True
    "grn" -> True
    "hzl" -> True
    "oth" -> True
    _     -> False

split :: String -> [String]
split contents = foldr split' [""] (lines contents) where
  split' :: String -> [String] -> [String]
  split' "" xs       = "" : xs
  split' s  (x : xs) = (s ++ ' ' : x) : xs
