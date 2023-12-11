
main :: IO ()
main = do
    inp <- getContents
    let (part2, part1) = solve inp
    putStrLn $ "Part 1: " ++ (show part1)
    putStrLn $ "Part 2: " ++ (show part2)

solve :: String -> (Int, Int)
solve = foldl1 add2 . map (foldl (\(l, r) xs -> (head xs - l, r + last xs)) (0, 0) . reverse . differenceList . map read . words) . lines

differenceList :: [Int] -> [[Int]]
differenceList [] = []
differenceList x  = x : differenceList x'
    where x' = zipWith (-) (tail x) x

add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
