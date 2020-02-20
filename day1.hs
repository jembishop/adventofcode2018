import qualified Data.Set as S

main = do 
    contents <- readFile "input/1.txt" 
    print $ day1 $ parse contents
    print $ day2 $ parse contents
    
parse :: String -> [Integer]
parse raw = map read (lines (filter (/='+') raw))

day1 :: [Integer] -> Integer
day1 = sum 

firstRepeated :: [Integer] -> Integer
firstRepeated = go S.empty
    where 
        go :: S.Set Integer -> [Integer] -> Integer
        go seen (x:xs) 
            | S.member x seen = x
            | otherwise = go (S.insert x seen) xs

day2 :: [Integer] -> Integer
day2 = firstRepeated . scanl (+) 0 . cycle
        
