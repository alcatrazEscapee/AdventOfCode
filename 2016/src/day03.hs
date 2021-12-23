
main :: IO ()
main = do
    inp <- getContents
    let ints = map (map int . words) . lines $ inp

    putStrLn $ "Part 1: " ++ (part1 ints)
    putStrLn $ "Part 2: " ++ (part2 ints)

part1 :: [[Int]] -> String
part1 = show . triangles

-- The sequence of (take3 . concat . transpose) effectively takes an Nx3 -> 3xN -> 3*N -> Nx3 list
part2 :: [[Int]] -> String
part2 = part1 . take3 . concat . transpose

-- Counts the number of triangles in the given list of (a, b, c) triples
triangles :: [[Int]] -> Int
triangles = sum . map (fromEnum . tri)
    where tri (a:b:c:_) = a + b > c && a + c > b && b + c > a

take3 :: [a] -> [[a]]
take3 [] = []
take3 ls = (take 3 ls) : (take3 . drop 3 $ ls)

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

int :: String -> Int
int s = read s ::Int
