import Data.Function
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Debug.Trace

main :: IO ()
main = do 
    contents <- readFile "input/3.txt" 
    print $ part1 $  parse contents
    print $ part2 $ parse contents

type Coord = (Integer, Integer)
type IDCoord = (Integer, Coord)
type Claim = (Integer, Coord, Coord)

parse :: String -> [Claim]
parse raw = map (go . words) (lines raw)
    where go l =  (_id l, ((coords l)!!0, coords l !!1), (size l !!0, size l!!1))
          _id l = read $ filter (/='#') (l !! 0)
          coords l = map read $ split (filter (/=':') (l !! 2 )) ','
          size l = map read $ split (l !! 3) 'x'

claimToIDCoords :: Claim -> [IDCoord]
claimToIDCoords cl =  let (i, (x0,y0), (w, h)) = cl in [(i, (x, y))| x <-[x0..(x0 + w - 1)], y <- [y0..(y0+h -1 )]]

part1 :: [Claim] -> Int
part1 = length . M.filter ((>=2) .length) . occFreq . intercalate [] . map claimToIDCoords  

part2 :: [Claim] -> S.Set Integer
part2 cl = S.difference ids bad
         where bad = S.unions . map S.fromList . filter ((>1) . length) $ coordids
               coordids = M.elems . occFreq .  intercalate [] . map claimToIDCoords $ cl 
               ids = S.fromList $ map (\(x,_, _) -> x) cl



occFreq :: [IDCoord] -> M.Map Coord [Integer]
occFreq = go M.empty
    where go counts ((i, coord):xs) = go (M.insertWith (++) coord [i] counts) xs
          go counts [] = counts




split :: String -> Char -> [String]
split [] _ = [""]
split (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split cs delim