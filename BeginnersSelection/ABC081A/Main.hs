import Data.Char
main :: IO ()
main = do
    nums <- map digitToInt <$> getLine
    putStrLn $ show $ sum nums