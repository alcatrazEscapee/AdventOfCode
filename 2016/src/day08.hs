import qualified Data.Char as Char

import Text.ParserCombinators.ReadP

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
screenHeight = 6
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
          parse = map parseLine . lines
          parseLine = fst . head . readP_to_S pInstruction

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

-- Parser Tokens

pInstruction :: ReadP Instruction
pInstruction = do
    name <- pWord
    char ' '
    inst <- case name of
        "rect" -> pRectInstruction
        "rotate" -> pOtherInstruction
    return inst

pRectInstruction :: ReadP Instruction
pRectInstruction = do
    x <- pInt
    char 'x'
    y <- pInt
    return (Rect x y)

pOtherInstruction :: ReadP Instruction
pOtherInstruction = do
    rowOrCol <- pWord
    char ' '
    inst <- case rowOrCol of
        "row" -> pRowInstruction
        "column" -> pColInstruction
    return inst

pRowInstruction :: ReadP Instruction
pRowInstruction = do
    string "y="
    y <- pInt
    string " by "
    by <- pInt
    return (Row y by)

pColInstruction :: ReadP Instruction
pColInstruction = do
    string "x="
    x <- pInt
    string " by "
    by <- pInt
    return (Col x by)

pWord :: ReadP String
pWord = munch1 Char.isAlpha

pInt :: ReadP Int
pInt = do
    v <- munch1 Char.isDigit
    return (read v)