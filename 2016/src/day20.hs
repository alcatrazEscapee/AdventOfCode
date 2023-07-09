import Data.List
import Data.Maybe

-- A set of ranges of [a, b], possibly disjoint
type Ranges = [(Int, Int)]

main :: IO ()
main = do
    inp <- getContents
    let ranges = disjoint . parse $ inp
    putStrLn $ "Part 1: " ++ (part1 ranges)
    putStrLn $ "Part 2: " ++ (part2 ranges)

-- First open spot, since the ranges are sorted, is just the first range, top value, + 1
part1 :: Ranges -> String
part1 = show . (+) 1 . snd . head

-- Number of open ranges, since they are disjoint, is the sum of all differences between top and bottom
part2 :: Ranges -> String
part2 ranges = show . sum . zipWith (\(_, b) (c, _) -> c - b - 1) ranges $ (tail ranges)

parse :: String -> [(Int, Int)]
parse inp = map (pairMap read) . map (splitOn '-') . lines $ inp
    where splitOn c x = let (a, b) = splitAt (fromJust $ elemIndex c x) x in (a, tail b)
          pairMap f (a, b) = (f a, f b)

-- Converts a list of possibly disjoint ranges to a sorted list of disjoint ranges
disjoint :: Ranges -> Ranges
disjoint ranges = reverse . foldll foldMerge . sortBy byMin $ ranges
    where byMin (a, _) (b, _) = compare a b
          foldMerge bs a = (merge (head bs) a) ++ (tail bs)

foldll :: ([a] -> a -> [a]) -> [a] -> [a]
foldll f (x:xs) = foldl f [x] xs
foldll _ _ = error "foldll cannot be applied to an empty list"

-- Where a < c, merges two ranges (if possible), otherwise returns two disjoint ranges
-- The larger range will be at the head of the list
merge :: (Int, Int) -> (Int, Int) -> Ranges
merge (a, b) (c, d)
    | b < c - 1 = [(c, d), (a, b)] -- Ranges are disjoint, cannot merge
    | otherwise = [(a, max b d)] -- Ranges overlap, so we can merge