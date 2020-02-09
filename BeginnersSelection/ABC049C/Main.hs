import Data.Char
import Data.List
import Data.Maybe
main :: IO ()
main = do
  input <- getLine
  putStrLn $ isValied [input]

-- | 評価する
--
-- >>> isValied ["erasedream"]
-- "YES"
-- >>> isValied ["dreameraser"]
-- "YES"
-- >>> isValied ["huga", "dreamerer"]
-- "NO"
-- >>> isValied ["", "hoge"]
-- "YES"
-- 
isValied :: [String] -> String
isValied ans
  | "" `elem` ans = "YES"
  | null droppedQuery = "NO"
  | otherwise = isValied droppedQuery
  where
    droppedQuery = [ dropPrefix y x | x <- ans, y <- ["dream","dreamer","erase","eraser"], y `isPrefixOf` x]

-- | 先頭文字列削除
-- >>> dropPrefix "erase" "erasedream"
-- "dream"
dropPrefix :: String -> String -> String 
dropPrefix a b = fromMaybe b $ stripPrefix a b

