import qualified Data.Maybe as Maybe


main :: IO ()
main = do
    inp <- getContents
    let (seeds, maps) = parse . lines $ inp
    putStrLn $ "Part 1: " ++ (part1 maps seeds)
    putStrLn $ "Part 2: " ++ (part2 maps seeds)


parse :: [String] -> ([Int], [[[Int]]])
parse (x:xs) = (seeds, maps)
    where seeds = map read . drop 1 . words $ x
          maps = map (map (map read . words) . drop 1) . splitEmpty $ xs

          splitEmpty :: [String] -> [[String]]
          splitEmpty []     = []
          splitEmpty (_:xs) = a : splitEmpty b
               where (a, b) = break (== []) xs


part1 :: [[[Int]]] -> [Int] -> String
part1 maps = show . minimum . map (foldl1 (flip (.)) . map apply $ maps)
    where apply :: [[Int]] -> Int -> Int
          apply []                     x = x
          apply ((dest:src:len:[]):rs) x = if src <= x && x < src + len
                                           then x - src + dest
                                           else apply rs x


part2 :: [[[Int]]] -> [Int] -> String
part2 maps = show . minimum . map fst . concatMap (foldl1 (\f g -> concatMap g . f) . map apply $ maps) . take2
    where apply :: [[Int]] -> (Int, Int) -> [(Int, Int)]
          apply []                     x       = [x]
          apply ((dest:src:len:[]):rs) (x, xl) = if x >= src + len || x + xl <= src
                                                 then apply rs (x, xl)
                                                 else (intL - src + dest, intR - intL) : (concatMap (apply rs) . Maybe.catMaybes $ (mapL:mapR:[]))
                where -- [intL, intR) describes the intersection of the range and the interval, which must get remapped through this range
                      intL = max x src
                      intR = min (x + xl) (src + len)

                      -- mapL, mapR describe the left prefix, and right prefix of the interval, respectively
                      -- They don't intersect this range, and so they need to be called with `apply` recursively
                      mapL = if x        < intL then Just (x, intL - x)         else Nothing
                      mapR = if (x + xl) > intR then Just (intR, x + xl - intR) else Nothing

          take2 :: [a] -> [(a, a)]
          take2 []       = []
          take2 (a:b:xs) = (a, b) : take2 xs
