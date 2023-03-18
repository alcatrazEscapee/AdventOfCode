
type Node = (Int, Int, Int, Int) -- x, y, size, used

main :: IO ()
main = do
    inp <- getContents
    let nodes = parse inp
    putStrLn $ "Part 1: " ++ (part1 nodes)
    putStrLn $ "Part 2:\n" ++ (part2 nodes)

part1 :: [Node] -> String
part1 nodes = show . length . filter compatable $ [(l, r) | l <- nodes, r <- nodes]
    where compatable :: (Node, Node) -> Bool
          compatable ((lx, ly, _, lu), (rx, ry, rs, ru)) = (lx /= rx || ly /= ry) && lu /= 0 && lu <= (rs - ru)

-- This only prints the grid
-- The actual answer can be found visually by inspecting said grid as a pathfinding problem with two parts:
-- 1. Moving the hole to (0, maxX - 1)
-- 2. Swap once to place the hole behind the target
-- 3. 'Hopping' the target data all the way to the destination -> each step consists of 5 moves/move
part2 :: [Node] -> String
part2 nodes = init . unlines . partition (1 + maxY) . map repr $ nodes
    where maxY = foldl1 max . map (\(_, y, _, _) -> y) $ nodes
          maxX = foldl1 max . map(\(x, _, _, _) -> x) $ nodes

          repr :: Node -> Char
          repr (x, y, _, u)
            | x == 0 && y == 0    = 'T' -- Target
            | x == maxX && y == 0 = 'D' -- Data
            | u == 0              = '_'
            | u > 100             = '#'
            | otherwise           = '.'

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = (take n xs) : partition n (drop n xs)

parse :: String -> [Node]
parse = map (parse' . words . map (\c -> if c == '-' then ' ' else c)) . drop 2 . lines
    where parse' :: [String] -> Node
          parse' (_ : (_ : x) : (_ : y) : size : used : _ : _ : []) = (read x, read y, read . init $ size, read . init $ used)
          parse' _ = error "Invalid input line"