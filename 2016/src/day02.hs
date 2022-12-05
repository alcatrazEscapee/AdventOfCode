
type Point = (Int, Int)

-- A keypad is represented by two functions
-- check :: A predicate if a point is on the keypad
-- nameOf :: A function to name a valid point on the keypad
type KeypadCheck = Point -> Bool
type KeypadNameOf = Point -> Char

origin = (0, 0) :: Point
diagonalKeypad = "..1...234.56789.ABC...D.." :: String -- The keypad from part 2, as a 5x5 grid

main :: IO ()
main = do
    inp <- getContents
    putStrLn $ "Part 1: " ++ (part1 inp)
    putStrLn $ "Part 2: " ++ (part2 inp)

part1 :: String -> String
part1 = solve nameOf check
    where nameOf (x, y) = head . show $ 5 + x + 3 * y
          check (x, y) = -1 <= x && x <= 1 && -1 <= y && y <= 1

part2 :: String -> String
part2 = solve nameOf check
    where nameOf p = diagonalKeypad !! index p
          check p = 0 <= i && i < 25 && (diagonalKeypad !! i) /= '.'
              where i = index p
          index (x, y) = 12 + x + 5 * y

-- Solve the puzzle, for a given keypad, represented as a (nameOf, check) pair
solve :: KeypadNameOf -> KeypadCheck -> String -> String
solve nameOf check s = map nameOf . map (foldl (advance check) origin) . lines $ s

-- (check, point, direction) -> next valid point via check()
advance :: KeypadCheck -> Point -> Char -> Point
advance check p s = if check c
                    then c
                    else p
    where c = add p (dir s)

dir :: Char -> Point
dir c = case c of 'U' -> (0, -1)
                  'D' -> (0, 1)
                  'L' -> (-1, 0)
                  'R' -> (1, 0)

add :: Point -> Point -> Point
add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)
