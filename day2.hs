import qualified Data.Map.Strict as M

main = do 
    contents <- readFile "input/2.txt" 
    print $ day1 $ parse contents
    -- print $ day2 $ parse contents
    
parse :: String -> [String]
parse = lines 

counts :: String -> M.Map Char Integer
counts =  go M.empty
    where 
        go count [] = count 
        go count (x:xs) = go (M.insertWith (+) x 1 count) xs 

onlyX :: Integer -> M.Map Char Integer -> Bool
onlyX x c = not $ M.null $ M.filter (==x) c 

countXInList :: Integer -> [String] -> Int
countXInList x l = length $ filter id (map (onlyX x . counts) l)

day1 :: [String] -> Int
day1 dat = countXInList 2 dat * countXInList 3 dat
