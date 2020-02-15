import Text.Read (readMaybe)
import Control.Monad
import Control.Applicative
import Data.Char


main = getLine >>= putStrLn . maybeToStr . strToInt

-- | String から Int へ変換
--
-- >>> strToInt "123"
-- Just 123
-- >>> strToInt "012"
-- Just 12
-- >>> strToInt "0x2"
-- Nothing
strToInt :: String -> Maybe Int
strToInt (a:b:c:xs) 
  | isDigit a && isDigit b && isDigit c = readMaybe (a:b:c:xs)
  | otherwise = Nothing

-- | Maybe Int から String へ変換
--
-- >>> maybeToStr (Just 123)
-- "246"
-- >>> maybeToStr (Just 12)
-- "24"
-- >>> maybeToStr Nothing
-- "error"
maybeToStr :: Maybe Int -> String
maybeToStr Nothing = "error"
maybeToStr (Just x) = show $ x * 2