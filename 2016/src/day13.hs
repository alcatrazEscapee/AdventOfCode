import qualified Data.Set as Set

import Data.Bits

type Point2 = (Int, Int)
type Point3 = (Int, Int, Int)

main :: IO ()
main = do
    inp <- getContents
    let (p1, p2) = bfs (int inp) Set.empty [(1, 1, 0)] (-1, 0)
    putStrLn $ "Part 1: " ++ (show p1)
    putStrLn $ "Part 2: " ++ (show p2)

int :: String -> Int
int s = read s :: Int

-- (salt, seen, queue, (-1, 0)) -> (number of steps to reach (31, 39), total number of locations you can reach in at most 50 steps)
bfs :: Int -> Set.Set Point2 -> [Point3] -> (Int, Int) -> (Int, Int)
bfs _    _    []                (p1, p2) = (p1, p2)
bfs salt seen ((x, y, d):queue) (p1, p2) = if Set.member (x, y) seen
                                                then bfs salt seen  queue  (p1, p2)
                                                else bfs salt seen' queue' (p1', p2')
    where next = filter (\p -> let (x', y', _) = p in isOpen salt x' y') . neighbors $ (x, y, d) :: [Point3]

          seen' = Set.insert (x, y) seen
          queue' = queue ++ next

          p1' = if x == 31 && y == 39 && p1 == -1
                    then d
                    else p1
          p2' = if d <= 50
                    then p2 + 1
                    else p2

neighbors :: Point3 -> [Point3]
neighbors (x, y, d) = [(x + 1, y, d + 1), (x - 1, y, d + 1), (x, y + 1, d + 1), (x, y - 1, d + 1)]

isOpen :: Int -> Int -> Int -> Bool
isOpen salt x y = x >= 0 && y >= 0 && (bits .&. 1) == 0
    where hash = x * x + 3 * x + 2 * x * y + y + y * y + salt
          bits = popCount hash
