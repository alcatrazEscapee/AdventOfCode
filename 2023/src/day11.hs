import qualified Data.List as List


main :: IO ()
main = do
    inp <- getContents
    let inp' = lines inp
    putStrLn $ "Part 1: " ++ (solve inp' 2)
    putStrLn $ "Part 2: " ++ (solve inp' 1000000)


solve :: [[Char]] -> Int -> String
solve inp dN = show . sum $ [
        abs (x1 - x2) + abs (y1 - y2)
            | ((x1, y1):gs) <- List.tails [
                (xs !! x, ys !! y)
                    | (y, line) <- zip [0..] inp
                    , (x, c)    <- zip [0..] line
                    , c == '#'
            ]
            , (x2, y2) <- gs
        ]
    where xs = coordinates dN . transpose $ inp
          ys = coordinates dN inp


coordinates :: Int -> [[Char]] -> [Int]
coordinates dN = scanl (+) 0 . map (\x -> if all (== '.') x then dN else 1)

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x      = (map head x) : transpose (map tail x)
