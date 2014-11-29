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
--possibleAnswers = replicateM 9 letters --nine letter words

--"aaaaaaaaa" to "uuuuuuuuu"

--we can dermine the word at the given index in because
--we can convert to base36 and then map the value in letters
padZeros :: Int -> String -> String
padZeros n s
  | length s < n  = (replicate (n - length s) '0') ++ s
  | otherwise     = s

base16ToGramar :: String -> String
base16ToGramar s = map (\c ->
  case c of
    '0' -> 'a'
    '1' -> 'c'
    '2' -> 'd'
    '3' -> 'e'
    '4' -> 'g'
    '5' -> 'i'
    '6' -> 'l'
    '7' -> 'm'
    '8' -> 'n'
    '9' -> 'o'
    'a' -> 'p'
    'b' -> 'q'
    'c' -> 'r'
    'd' -> 's'
    'e' -> 't'
    'f' -> 'u'
    _ -> error $ show $ "Invalid base36: " ++ s
    ) $ padZeros 9 s -- Should be 9 chars long

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


    
