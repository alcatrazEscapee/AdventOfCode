
-- Sequences both inside and outside of square brackets
type IP = ([String], [String])


main :: IO ()
main = do
    inp <- getContents
    putStrLn $ "Part 1: " ++ (part1 inp)
    putStrLn $ "Part 2: " ++ (part2 inp)

part1 :: String -> String
part1 = solve supportsTLS sequencesABBA
    where -- A IP supports TLS if it contains ANY 'ABBA' outside of square brackets, and NO 'ABBA' inside of them
          supportsTLS (left, right) = (not . null $ left) && null right

part2 :: String -> String
part2 = solve supportsSSL sequencesABA
    where -- An IP supports SSL if it contains a 'ABA' outside of square brackets, and the matching 'BAB' inside of them
          -- Thus, we have to check each sequence, flip it, and check it's membership in the right side
          supportsSSL (left, right) = not . null . filter (\x -> elem x right) . map flipABA $ left


-- Solver
-- (predicate that checks if an IP subsequences are supported) -> (sequence splitter for an IP) -> (input) -> (count of supported IPs)
solve :: (IP -> Bool) -> (String -> [String]) -> String -> String
solve support seq = show . length . filter support . map mapParts . splitInto
    where mapParts (left, right) = (mapPart left, mapPart right)
          mapPart = concat . map seq


-- Extracts sequences of 'ABBA' from a string and returns the list of all such sequences
sequencesABBA :: String -> [String]
sequencesABBA ip = seq ip []
    where seq (a:b:c:d:xs) acc = if a == d && b == c && a /= b
                                 then seq (c:d:xs) ((a:b:c:d:[]) : acc)
                                 else seq (b:c:d:xs) acc
          seq (_:xs)       acc = seq xs acc
          seq []           acc = acc

-- Extracts sequences of 'ABA' from a string and returns the list of all such sequences
sequencesABA :: String -> [String]
sequencesABA ip = seq ip []
    where seq (a:b:c:xs) acc = if a == c && a /= b
                               then seq (b:c:xs) ((a:b:c:[]) : acc)
                               else seq (b:c:xs) acc
          seq (_:xs)     acc = seq xs acc
          seq []         acc = acc

-- Flips a string 'ABA' to 'BAB'
flipABA :: String -> String
flipABA (a:b:_) = (b:a:b:[])
flipABA xs = xs


-- Splits the input string into a list of IPs
-- Note that each IP will be in reverse order, which is fine since we're concerned with order-independent subsequences
splitInto :: String -> [IP]
splitInto s = map splitLineInto . lines $ s
    where splitLineInto line = splitLineInto' line [] ([], []) -- Non-recursive calls the recursive version
          -- End of input
          -- Insert the last string into the accumulator
          splitLineInto' [] acc (left, right) = (acc : left, right)
          -- Brackets swap the hash order, and append the current accumulator
          -- They are always correctly matched so we don't have to worry about ending up swapped
          splitLineInto' ('[' : xs) acc (left, right) = splitLineInto' xs [] (right, acc : left)
          splitLineInto' (']' : xs) acc (left, right) = splitLineInto' xs [] (right, acc : left)
          -- Base case - accumulate a single character in the accumulator
          splitLineInto' (x : xs) acc (left, right) = splitLineInto' xs (x : acc) (left, right)
          