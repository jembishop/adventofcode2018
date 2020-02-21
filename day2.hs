import qualified Data.Map.Strict as M
import Data.List (tails, sort)
import Data.Maybe (fromJust)

main = do 
    contents <- readFile "input/2.txt" 
    print $ day1 $ parse contents
    print $ day2 $ parse contents
    
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

diffBy1Char :: String -> String -> Maybe (Char, Char)
diffBy1Char x y = if length diffs == 1 then Just (head diffs) else Nothing
    where diffs = filter (uncurry (/=)) $ zip x y

diffBy1CharInList :: [String] -> (String, String)
diffBy1CharInList l = (so !! idx, so !! (idx + 1))
    where idx = snd . head $ filter ((/=Nothing) . fst) (zip (zipWith diffBy1Char so (tail so)) [0..])
          so = sort l

day2 :: [String] -> String 
day2 dat = filter (/=char) (fst diffWords) 
    where char = fst (fromJust (uncurry diffBy1Char diffWords)) 
          diffWords = diffBy1CharInList dat
