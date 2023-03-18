import qualified Data.List as List
import qualified Data.Maybe as Maybe

-- Each line of the input is an instruction
-- 1. Turn on Rect (x, y), 2. Rotate Row x by <num>, 3. Rotate Col y by <num>
data Instruction = 
    Rect Int Int |
    Row Int Int |
    Col Int Int
    deriving (Show)

-- All states start off
data State = On | Off
    deriving (Eq)

-- In a two dimensional screen
type Screen = [[State]]

-- Of 6 x 50 pixels
screenHeight :: Int
screenHeight = 6

screenWidth :: Int
screenWidth = 50

main :: IO ()
main = do
    inp <- getContents
    let screen = parseAndExecuteInstructions $ inp
    putStrLn $ "Part 1: " ++ (show . totalPixelsTurnedOn $ screen) ++ "\nPart 2:"
    putStrLn $ screenAsText screen

parseAndExecuteInstructions :: String -> Screen
parseAndExecuteInstructions = foldl execInstruction start . parse
    where start = replicate screenHeight (replicate screenWidth Off)
          parse = map (parse' . words) . lines
          
          parse' :: [String] -> Instruction
          parse' ("rect" : xy : []) = Rect (read . take n $ xy) (read . drop (n + 1) $ xy)
                where n = Maybe.fromJust . List.elemIndex 'x' $ xy
          parse' ("rotate" : "row"    : (_:_:y) : _ : by : []) = Row (read y) (read by)
          parse' ("rotate" : "column" : (_:_:x) : _ : by : []) = Col (read x) (read by)
          parse' _ = error "Invalid input line"

totalPixelsTurnedOn :: Screen -> Int
totalPixelsTurnedOn = sum . map (length . filter (== On))

screenAsText :: Screen -> String
screenAsText screen = foldl joinOnNewLine "" (mapCol screen)
    where joinOnNewLine acc x = acc ++ "\n" ++ x
          mapCol = map mapRow
          mapRow = map mapOne
          mapOne p = case p of
                On -> '#'
                Off -> '.'

execInstruction :: Screen -> Instruction -> Screen
execInstruction screen (Rect x y) = (map turnOnRow . (take y) $ screen) ++ (drop y screen)
    where turnOnRow row = (replicate x On) ++ drop x row

-- Only implements by-row rotations as it's easier
-- By-column rotations are done by transposing, rotating, and transposing again
execInstruction screen (Row y by) = rotateByRow y by screenWidth screen
execInstruction screen (Col x by) = transpose . rotateByRow x by screenHeight . transpose $ screen

rotateByRow :: Int -> Int -> Int -> Screen -> Screen
rotateByRow y by width screen = (take y screen) ++ [newRow] ++ (drop (y + 1) screen)
    where oldRow = head . drop y $ screen 
          newRow = (drop mBy oldRow) ++ (take mBy oldRow)
          mBy = width - by

-- Borrowed from Day 4
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)
