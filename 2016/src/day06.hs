import Data.Ord

import qualified Data.List as List
import qualified Data.Map as Map

main :: IO ()
main = do
    inp <- getContents
    let freq = map characterFrequencies . transpose . lines $ inp
    putStrLn $ "Part 1: " ++ (map last freq)
    putStrLn $ "Part 2: " ++ (map head freq)

characterFrequencies :: String -> [Char] 
characterFrequencies = map fst . List.sortBy (comparing snd) . Map.toList . foldl account Map.empty
    where account :: Map.Map Char Int -> Char -> Map.Map Char Int
          account counter c = Map.insertWith (+) c 1 counter

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)