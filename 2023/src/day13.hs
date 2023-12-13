import qualified Data.List as List
import qualified Data.Maybe as Maybe

type Grid = [[Char]]
type Predicate = [(String, String)] -> Bool

main :: IO ()
main = do
    inp <- getContents
    let grids = splitEmpty . lines $ inp
    putStrLn $ "Part 1: " ++ (solve part1 grids)
    putStrLn $ "Part 2: " ++ (solve part2 grids)


splitEmpty :: [String] -> [[String]]
splitEmpty xs = case break (== []) xs of
                        (a, _:bs) -> a : splitEmpty bs
                        (a, []) -> [a]

solve :: Predicate -> [Grid] -> String
solve f = show . sum . map (\grid -> 100 * (reflect grid) + (reflect . List.transpose $ grid))
    where reflect :: Grid -> Int
          reflect grid = Maybe.fromMaybe 0 . Maybe.listToMaybe . filter (\n -> f $ zip (reverse $ take n grid) (drop n grid)) $ [1..length grid - 1]

part1 :: Predicate
part1 = all $ uncurry (==)

part2 :: Predicate
part2 = (==) 1 . sum . map (length . filter (uncurry (/=)) . uncurry zip)
