import qualified Numeric as Numeric


main :: IO ()
main = do
    inp <- getContents
    let (part1, part2) = solve inp
    putStrLn $ "Part 1: " ++ part1
    putStrLn $ "Part 2: " ++ part2

solve :: String -> (String, String)
solve = mapPair (show . area) . unzip . reverse . map (parseLine . words) . lines
    where parseLine :: [String] -> ((Int, Int), (Int, Int))
          parseLine ((k1:[]):n1:(_:_:k2):[]) = (parseKey k1 (read n1), (parseKey (head . drop 5 $ k2) (fst . head . Numeric.readHex . take 5 $ k2)))
          parseLine _                        = error "Invalid line"
          
          parseKey :: Char -> Int -> (Int, Int) 
          parseKey k n
            | k == 'R' || k == '0' = (0, n)
            | k == 'D' || k == '1' = (n, 0)
            | k == 'L' || k == '2' = (0, -n)
            | k == 'U' || k == '3' = (-n, 0)
            | otherwise            = error "Invalid key"

-- Calculates the area of a polygon using Green's Theorem
-- This is a more mathematical solution than the coordinate subdivision method used in Cordy
-- Motivated by https://www.reddit.com/r/adventofcode/comments/18l0qtr/2023_day_18_solutions/kduuicl/
area :: [(Int, Int)] -> Int
area = area' 0 0 0 0
    where area' :: Int -> Int -> Int -> Int -> [(Int, Int)] -> Int
          area' _ _ a p []               = a + (p `div` 2) + 1
          area' x y a p ((dx, dy):xs)    = area' x' y' a' p' xs
                where x' = x + dx
                      y' = y + dy
                      p' = p + (abs dx) + (abs dy)
                      a' = a + x' * dy

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)