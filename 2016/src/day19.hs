
main :: IO ()
main = do
    inp <- getContents
    let size = int inp
    putStrLn $ "Part 1: " ++ (part1 size)
    putStrLn $ "Part 2: " ++ (part2 size)

part1 :: Int -> String
part1 size = show . play . ring $ [1..size]
    where play :: Ring Int -> Int
          play r = if unit r
                   then let (Ring _ c _) = r in c
                   else play $ popL . rotL $ r

part2 :: Int -> String
part2 size = show . play (size `mod` 2 == 1) . composeN rotL (size `div` 2) . ring $ [1..size]
    where play :: Bool -> Ring Int -> Int
          play p r = if unit r
                     then let (Ring _ c _) = r in c
                     else play (not p) $ (if p then rotL else id) . popL $ r

int :: String -> Int
int s = read s :: Int

composeN :: (a -> a) -> Int -> a -> a
composeN f n x = iterate f x !! n


-- A Ring - a cyclic list with O(1) rotL, popL (amortized) operations
data Ring a = Ring [a] a [a]
    deriving (Show)

ring :: [a] -> Ring a
ring [] = error "Cannot construct an empty ring"
ring (x:xs) = (Ring [] x xs)

rotL :: Ring a -> Ring a
rotL r@(Ring [] _ []) = r
rotL (Ring l c []) = let (r:rs) = reverse (c:l) in Ring [] r rs
rotL (Ring l c (r:rs)) = Ring (c : l) r rs

popL :: Ring a -> Ring a
popL (Ring [] _ []) = error "Cannot pop to an empty ring"
popL (Ring l _ []) = let (r:rs) = reverse l in (Ring [] r rs)
popL (Ring l _ (r:rs)) = (Ring l r rs)

unit :: Ring a -> Bool
unit (Ring l _ r) = null l && null r
