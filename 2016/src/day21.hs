import Data.List
import Data.Maybe

data Instruction =
    SwapPosition Int Int |
    SwapLetter Char Char |
    RotateLeft Int |
    RotateRight Int |
    RotatePositional Char |
    ReversePositions Int Int |
    MovePosition Int Int
    deriving(Show)

main :: IO ()
main = do
    inp <- getContents
    let rules = parse inp
    putStrLn $ "Part 1: " ++ (foldl (apply False) "abcdefgh" rules)
    putStrLn $ "Part 2: " ++ (foldl (apply True) "fbgdceah" (reverse rules))

apply :: Bool -> String -> Instruction -> String
apply _ s (SwapPosition x y)     = swapPosition s x y
apply _ s (SwapLetter x y)       = swapLetter s x y
apply r s (RotateLeft x)         = (if r then rotateRight else rotateLeft) s x
apply r s (RotateRight x)        = (if r then rotateLeft else rotateRight) s x
apply r s (RotatePositional x)   = (if r then rotatePositionalRev else rotatePositional) s x
apply _ s (ReversePositions x y) = reverseRange s x y
apply r s (MovePosition x y)     = if r then movePosition s y x else movePosition s x y

-- Swap position, since all letters are unique, just indexes and then calls swapLetter
swapPosition :: String -> Int -> Int -> String
swapPosition s x y = swapLetter s (s !! x) (s !! y)

-- Swap letter can be done via simple iteration, mapping the letter to it's swapped version, or itself
swapLetter :: String -> Char -> Char -> String
swapLetter s x y = map (\c -> if c == x then y else if c == y then x else c) s

-- Rotates the entire string left, by drop + take
-- Right rotations are handled as left rotations by (length - x)
rotateLeft :: String -> Int -> String
rotateLeft s x = (drop x' s) ++ (take x' s)
    where x' = x `mod` (length s)

rotateRight :: String -> Int -> String
rotateRight s x = rotateLeft s ((length s) - x)

-- Rotate positional is a right rotation by a specific index which can be computed
rotatePositional :: String -> Char -> String
rotatePositional s x = rotateRight s (i + (if i >= 4 then 2 else 1))
    where i = fromJust . elemIndex x $ s

-- Reverse rotate positional uses the structure of rotate positional, to map the index and then do a rotate left with that index
rotatePositionalRev :: String -> Char -> String
rotatePositionalRev s x = rotateLeft s i'
    where i  = fromJust . elemIndex x $ s
          i' = case i of 
                1 -> 1
                3 -> 2
                5 -> 3
                7 -> 4
                2 -> 6
                4 -> 7
                6 -> 8
                0 -> 9
                _ -> error "Invalid index"

-- Reverse a sub-range, by constructing a new list from prefix, reversed, and suffix
-- We assume the sub-range is ordered (x, y) s.t. x < y.
reverseRange :: String -> Int -> Int -> String
reverseRange s x y = (take x s) ++ (reverse . take (y - x + 1) . drop x $ s) ++ (drop (y + 1) s)

movePosition :: String -> Int -> Int -> String
movePosition s x y = (take y s') ++ [c] ++ (drop y s') -- Join with the character in the correct index
    where c = s !! x -- Identify the character to be removed
          s' = filter (/= c) s -- And filter it out


parse :: String -> [Instruction]
parse = map (parse' . words) . lines
    where parse' :: [String] -> Instruction
          parse' ("swap" : "position" : x : _ : _ : y         : []) = SwapPosition (read x) (read y)
          parse' ("swap" : "letter" : (x:[]) : _ : _ : (y:[]) : []) = SwapLetter x y
          parse' ("rotate" : "left" : x : _                   : []) = RotateLeft (read x)
          parse' ("rotate" : "right" : x : _                  : []) = RotateRight (read x)
          parse' ("rotate" : "based" : _ : _ : _ : _ : (x:[]) : []) = RotatePositional x
          parse' ("reverse" : "positions" : x : _ : y         : []) = ReversePositions (read x) (read y)
          parse' ("move" : "position" : x : _ : _ : y         : []) = MovePosition (read x) (read y)
          parse' _ = error "Invalid instruction"
