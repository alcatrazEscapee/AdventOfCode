import qualified Data.Set as Set

type Point = (Int, Int)

start :: Point
start = (0, 0)
north :: Point
north = (0, 1)

main :: IO ()
main = do
    inp <- getContents
    putStrLn $ "Part 1: " ++ (part1 inp)
    putStrLn $ "Part 2: " ++ (part2 inp)

part1 :: String -> String
part1 inp = show . norm1 . last . walk $ inp

part2 :: String -> String
part2 inp = show . norm1 $ (dup points)
    where points = acc [start] (tail . walk $ inp)
          acc xs [] = xs
          acc xs (c:cs) = acc (xs ++ project (last xs) c) cs

walk :: String -> [Point]
walk inp = map fst corners
    where corners = scanl step (start, north) directions
          directions = words . strip ',' $ inp

step :: (Point, Point) -> String -> (Point, Point)
step (p, dp) s = (add p . mul dp' . int . tail $ s, dp')
    where dp' = rot dp (head s)

-- (points) -> first duplicate point
dup :: [Point] -> Point
dup xs = dup' xs Set.empty
    where dup' (x:xs') seen = if Set.member x seen
                             then x
                             else dup' xs' (Set.insert x seen)
          dup' _ _ = error "No duplicate points"

-- (c, string) -> string with all instances of 'c' removed
strip :: Char -> String -> String
strip _ [] = []
strip c (x : xs) = if x == c
                   then t
                   else x : t
    where t = strip c xs

-- (p, q) -> [projection of points along the line pq]
-- Handles only horizontal, vertical, or exactly diagonal lines
-- Includes q but not p
project :: Point -> Point -> [Point]
project (px, py) (qx, qy) = [(px + sx * t, py + sy * t) | t <- [1..max (abs dx) (abs dy)]]
    where dx = qx - px
          dy = qy - py
          sx = signum dx
          sy = signum dy

norm1 :: Point -> Int
norm1 (x, y) = abs x + abs y

add :: Point -> Point -> Point
add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

mul :: Point -> Int -> Point
mul (x, y) s = (x * s, y * s)

rot :: Point -> Char -> Point
rot (x, y) 'L' = (-y, x)
rot (x, y) 'R' = (y, -x)
rot _ _ = error "Invalid rotation"

int :: String -> Int
int s = read s ::Int
