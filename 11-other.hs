import Data.List

letters = "acdegilmnoprstuw"

--define our hadh function
hash = foldl (\x y -> x*37 + (findIndex' y)) 7
findIndex' y =
  case elemIndex y letters of
    Nothing -> -1 -- To match JS
    Just z -> z

-- Our word list generator function
--  ex:
--    getWordAtIndex 1 <==> "a"
--    getWordAtIndex 16 <==> "ca"
getWordAtIndex i = convertFromDecimal i ""
convertFromDecimal num accum
  | num == 0 = accum :: String
  | num > 0  =
    let
        over   = num `mod` 16
        remain = num `div` 16
        accum' = (letters !! over) : accum
    in
    convertFromDecimal remain accum'

hashAtIndex = hash . getWordAtIndex

-- http://en.wikipedia.org/wiki/Exponential_search
exponentialSearch :: Int -> Int -> Int -> Maybe Int
exponentialSearch value lower upper
  | hashAtIndex upper < value = exponentialSearch value upper (upper*2)
  | otherwise                 = binarySearch value lower upper

binarySearch value lower upper
  | upper   < lower           = Nothing
  | hashAtIndex(mid) < value  = binarySearch value (mid+1) upper
  | hashAtIndex(mid) > value  = binarySearch value lower (mid-1)
  | otherwise                 = Just mid
  where
  mid = lower + ((upper - lower) `div` 2)

main = do
  let toFind = 956446786872726
  case exponentialSearch toFind 0 1 of
    Nothing -> error $ "No word exists with hash " ++ show(toFind)
    Just z  -> putStrLn $ show $ getWordAtIndex z
