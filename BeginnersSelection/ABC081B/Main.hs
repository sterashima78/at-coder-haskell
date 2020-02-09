import Data.Char
main :: IO ()
main = do
    getLine
    entries <- map read . words <$> getLine
    putStrLn $ show $ execCount 0 entries

execCount :: Int -> [Int] -> Int
execCount count entries
    | (length entries) == (numOfEvenEven entries) = execCount (count + 1) (divBy2 entries)
    | otherwise = count 

divBy2 :: [Int] -> [Int]
divBy2 x = map (`div` 2) x 

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

numOfEvenEven :: [Int] -> Int
numOfEvenEven x = length $ filter isEven x
