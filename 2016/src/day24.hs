import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Data.Maybe

type Grid = (Int, Int, Array.Array Int Char)
type Point = (Int, Int)

type PathMap = Map.Map Char Int

type Graph = Map.Map Char (PathMap)
type VisitSet = Set.Set Char


main :: IO ()
main = do
    inp <- getContents
    let (graph, paths) = solve inp
    putStrLn $ "Part 1: " ++ (part1 paths)
    putStrLn $ "Part 2: " ++ (part2 graph paths)


part1 :: PathMap -> String
part1 paths = show . foldl1 min . map (\(_, v) -> v) . Map.toList $ paths

part2 :: Graph -> PathMap -> String
part2 graph paths = show . foldl1 min . map (\(k, v) -> v + (distBackToZero k)) . Map.toList $ paths
    where distBackToZero c = fromJust . Map.lookup c . fromJust . Map.lookup '0' $ graph

solve :: String -> (Graph, PathMap)
solve inp = (graph, paths)
    where (grid, points) = parseGrid inp

          graph = Map.fromList . map (\(p, c) -> (c, pathsFrom p)) . Map.toList $ points
          visits = Set.fromList . map (\(_, v) -> v) . Map.toList $ points
          paths = tsp graph (Set.singleton ('0', visits, 0)) Map.empty

          -- Find the map of all paths from x -> (y, distance)
          pathsFrom x = bfs grid points Set.empty (Seq.singleton (x, 0)) Map.empty


tsp :: Graph -> Set.Set (Char, VisitSet, Int) -> Map.Map (Char, VisitSet) Int -> PathMap
tsp graph queue paths
    -- If the queue is empty, then return the map of minimal paths
    | Set.null queue           = Map.mapKeys (\(c, _) -> c) . Map.filterWithKey (\(_, s) _ -> Set.null s) $ paths
    -- If the current queue entry is larger than the best known distance in the map, skip and recurse with the tail of the queue
    | prevIsWorseThanBestKnown = tsp graph queueTail paths
    -- Otherwise, explore the head of the queue
    | otherwise                = tsp graph queue' paths'
    where ((prevC, prevVisited, prevDist), queueTail) = Set.deleteFindMin queue

          nextPoints = map visit . filter (\c -> Set.member c prevVisited) . Map.keys $ graph

          queue' = Set.union queueTail (Set.fromList nextPoints)
          paths' = Map.insertWith min (prevC, prevVisited) prevDist paths

          -- Visit the point 'c' from the previous state
          visit :: Char -> (Char, VisitSet, Int)
          visit c = (c, Set.delete c prevVisited, prevDist + distPrev2C)
                where distPrev2C = fromJust . Map.lookup c . fromJust . Map.lookup prevC $ graph

          prevIsWorseThanBestKnown = maybe False (\mapDist -> mapDist < prevDist) $ Map.lookup (prevC, prevVisited) paths

bfs :: Grid -> Map.Map Point Char -> Set.Set Point -> Seq.Seq (Point, Int) -> PathMap -> PathMap
bfs grid points seen queue paths
    -- If the queue is empty, then return the explored paths
    | Seq.null queue                    = paths
    -- Additional early exit: if the length of the paths map is equal to the length of the points map,
    -- we must have already reached all reachable points, thus
    | Map.size paths == Map.size points = paths
    -- If we've seen the current point before, then recurse with the tail of the queue
    | Set.member prev seen              = bfs grid points seen queueTail paths
    -- Otherwise, explore the head of the queue
    | otherwise                         = bfs grid points seen' queue' paths'
    where ((prev@(x, y), prevDist), queueTail) = let (h Seq.:< t) = Seq.viewl queue in (h, t)
        
          nextPoints = filter (\((x', y'), _) -> (gridAt grid x' y') == '.') . map (\(dx, dy) -> ((x + dx, y + dy), prevDist + 1)) $ [(-1, 0), (1, 0), (0, -1), (0, 1)]

          seen' = Set.insert prev seen
          queue' = queueTail Seq.>< (Seq.fromList nextPoints)
          paths' = maybe paths (\c -> Map.insertWith min c prevDist paths) $  Map.lookup prev points

gridAt :: Grid -> Int -> Int -> Char
gridAt (w, h, grid) x y = if 0 <= x && x < w && 0 <= y && y < h
                          then grid Array.! (x + w * y)
                          else '#'

gridIndex :: Grid -> Int -> (Int, Int)
gridIndex (w, _, _) index = (x, y)
    where x = index `mod` w
          y = index `div` w

parseGrid :: String -> (Grid, Map.Map Point Char)
parseGrid inp = (grid, points)
    where grid = (width, height, array)
          array = Array.listArray (0, width * height - 1) . map skipNumbers . concat . lines $ inp
          points = Map.fromList . map (\x -> let (i, a) = x in (gridIndex grid i, a)) . filter onlyNumbers . zip [0..] . concat . lines $ inp

          width = length . head . lines $ inp
          height = length . lines $ inp

          skipNumbers a = if a == '#' then '#' else '.'
          onlyNumbers (_, a) = a /= '#' && a /= '.'
