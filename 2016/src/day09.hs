import qualified Data.Char as Char

import Text.ParserCombinators.ReadP

main :: IO ()
main = do
    inp <- getContents
    putStrLn $ "Part 1: " ++ (show $ part1 inp 0)
    putStrLn $ "Part 2: " ++ (show $ part2 inp 0)

part1 :: String -> Int -> Int
part1 []     n = n
part1 (x:xs) n = if x == '('
                 -- If we find a marker, increment our length accumulator by len * mul
                 -- Then drop the next `len` characters from the input, after the marker
                 then part1 (drop len xs') (n + len * mul)
                 -- Otherwise, increment and skip this character
                 else part1 xs (n + 1)
    where ((len, mul), xs') = parseMarker xs

part2 :: String -> Int -> Int
part2 []     n = n
part2 (x:xs) n = if x == '('
                 -- If we find a marker, we need to recursively include the total length of all expanded markers
                 -- We do this by calling part2 again, on the slice of length `len`
                 then let len' = part2 (take len xs') 0 in part2 (drop len xs') (n + mul * len')
                 -- Otherwise, increment and skip this character
                 else part2 xs (n + 1)
    where ((len, mul), xs') = parseMarker xs


-- Where the prefix of a string is '(', this will parse a marker `(AxB)`, returning (A, B) and the remaining string after the marker
parseMarker :: String -> ((Int, Int), String)
parseMarker x = head . readP_to_S pMarker $ x
    where pMarker :: ReadP (Int, Int)
          pMarker = do
                len <- munch1 Char.isDigit
                _ <- char 'x'
                mul <- munch1 Char.isDigit
                _ <- char ')'
                return (read len, read mul)
