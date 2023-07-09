import qualified Data.Set as Set
import qualified Data.Sequence as Seq

type Node = (Int, Int, Int, Int) -- x, y, size, used
type Point = (Int, Int) -- x, y

main :: IO ()
main = do
    inp <- getContents
    let nodes = parse inp
    putStrLn $ "Part 1: " ++ (part1 nodes)
    putStrLn $ let (display, answer) = part2 nodes in "Part 2: " ++ answer ++ "\n" ++ display ++ "\n"

part1 :: [Node] -> String
part1 nodes = show . length . filter compatable $ [(l, r) | l <- nodes, r <- nodes]
    where compatable :: (Node, Node) -> Bool
          compatable ((lx, ly, _, lu), (rx, ry, rs, ru)) = (lx /= rx || ly /= ry) && lu /= 0 && lu <= (rs - ru)

-- Returns (a 2D representation of the grid, the answer to part 2)
-- The answer is found via the procedure:
-- 1. Moving the hole to (0, maxX - 1) - this is done via a simple BFS
-- 2. Swap once to place the hole behind the target
-- 3. 'Hopping' the target data all the way to the destination -> each step consists of 5 moves/move
part2 :: [Node] -> (String, String)
part2 nodes = (display, show answer)
    where maxY = foldl1 max . map (\(_, y, _, _) -> y) $ nodes
          maxX = foldl1 max . map (\(x, _, _, _) -> x) $ nodes

          repr :: Node -> Char
          repr (x, y, _, u)
            | x == 0 && y == 0    = 'T' -- Target
            | x == maxX && y == 0 = 'D' -- Data
            | u == 0              = '_'
            | u > 100             = '#'
            | otherwise           = '.'
        
          walls :: Set.Set Point
          walls = Set.fromList . map (\(x, y, _, _) -> (x, y)) . filter (\p -> '#' == (repr p)) $ nodes

          hole :: Point
          hole = head . map (\(x, y, _, _) -> (x, y)) . filter (\p -> '_' == (repr p)) $ nodes

          answer = 1 + 5 * (maxX - 1) + (bfs (maxX - 1, 0) (maxX, maxY, walls) Set.empty (Seq.fromList [(hole, 0)]))
          display = init . unlines . partition (1 + maxY) . map repr $ nodes

bfs :: Point -> (Int, Int, Set.Set Point) -> Set.Set Point -> Seq.Seq (Point, Int) -> Int
bfs target walls seen queue
    -- If the queue is empty, then error
    | Seq.null queue       = error "BFS terminated without reaching the target"
    -- If we reach the target, then return the distance to said target
    | target == prev       = prevDist
    -- If we've seen the current point before, then recurse with the tail of the queue
    | Set.member prev seen = bfs target walls seen queueTail
    -- Otherwise, explore the head of the queue
    | otherwise            = bfs target walls seen' queue'
    where ((prev@(x, y), prevDist), queueTail) = let (h Seq.:< t) = Seq.viewl queue in (h, t)
        
          nextPoints = map (\p -> (p, prevDist + 1)) . filter isLegal . map (\(dx, dy) -> (x + dx, y + dy)) $ [(-1, 0), (1, 0), (0, -1), (0, 1)]

          isLegal :: Point -> Bool
          isLegal p@(x', y') = let (w, h, walls') = walls in x' >= 0 && y' >= 0 && x' <= w && y' <= h && not (Set.member p walls')

          seen' = Set.insert prev seen
          queue' = queueTail Seq.>< (Seq.fromList nextPoints)


partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = (take n xs) : partition n (drop n xs)

parse :: String -> [Node]
parse = map (parse' . words . map (\c -> if c == '-' then ' ' else c)) . drop 2 . lines
    where parse' :: [String] -> Node
          parse' (_ : (_ : x) : (_ : y) : size : used : _ : _ : []) = (read x, read y, read . init $ size, read . init $ used)
          parse' _ = error "Invalid input line"