import qualified Data.Set as S

main = do 
    contents <- readFile "input/1.txt" 
    print $ part1 $ parse contents
    print $ part2 $ parse contents
    
parse :: String -> [Integer]
parse raw = map read (lines (filter (/='+') raw))

part1 :: [Integer] -> Integer
part1 = sum 

firstRepeated :: [Integer] -> Integer
firstRepeated = go S.empty
    where 
        go :: S.Set Integer -> [Integer] -> Integer
        go seen (x:xs) 
            | S.member x seen = x
            | otherwise = go (S.insert x seen) xs

part2 :: [Integer] -> Integer
part2 = firstRepeated . scanl (+) 0 . cycle
        
