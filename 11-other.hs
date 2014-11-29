import Data.List
import Control.Monad
import Numeric
import Data.Char

letters = "acdegilmnoprstuw"

findIdx :: Char -> Int
findIdx y = case elemIndex y letters of
              Nothing -> -1
              Just z -> z

hash :: String -> Int
hash = foldl (\x y -> x*37 + (findIdx y)) 7

--it is a nine letter word
--"aaaaaaaaa" to "wwwwwwwww"

--we can dermine the word at the given index in because
--we can convert to base36 and then map the value in letters
padZeros :: Int -> String -> String
padZeros n s
  | length s < n  = (replicate (n - length s) '0') ++ s
  | otherwise     = s

convertFromDecimal :: Int -> Int -> String -> String
convertFromDecimal num toBase accum
  | num < 0     = error "number cannot be negative"
  | num == 0 = accum :: String
  | num > 0  =
    let chars  = "acdegilmnoprstuw"
        over   = num `mod` toBase
        remain = num `div` toBase
        accum' = (chars !! over) : accum
    in
    convertFromDecimal remain toBase accum'

getWordAtOffset i = padZeros 9 $ convertFromDecimal i 16 ""
hashAt = hash . getWordAtOffset

expValues = map (^2) [1..]

expSearch :: Int -> Int -> Int -> (Int, Int)
expSearch value lower upper | hashAt upper < value = expSearch value upper (upper*2)
                            | otherwise            = (lower, upper)


main = do
  case findIndex (\x -> hashAt(x) > 956446786872726) expValues of
    Nothing -> error "Unable to find"
    Just z -> putStrLn $ show $ expValues !! z
