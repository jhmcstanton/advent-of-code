import Control.Monad.Fix (fix)

main = do
  components <- componentLines <$> getContents
  let componentFuel = componentCounterUpper components
  putStrLn $ show componentFuel
  let fuelFuel = fuelCounterUpper components
  putStrLn $ show fuelFuel

type Components = [Integer]

componentLines :: String -> Components
componentLines = fmap read . takeWhile (/= "") . lines

-- Part 1
componentCounterUpper :: Components -> Integer
componentCounterUpper = sum . map fuelAmount

-- Part 2
fuelCounterUpper :: Components -> Integer
fuelCounterUpper = sum . map (fuelFuel . fuelAmount) where
  fuelFuel n = if n <= 0 then 0 else fuelFuel (fuelAmount n) + n

fuelAmount :: Integer -> Integer
fuelAmount x = (x `div` 3) - 2
