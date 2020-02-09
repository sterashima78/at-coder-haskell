import Data.Char
main :: IO ()
main = do
    nums <- map digitToInt <$> getLine
    print $ sum nums