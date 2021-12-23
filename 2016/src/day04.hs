import Data.Ord

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map

import Text.ParserCombinators.ReadP

-- Room Name (no spaces), Sector ID, Checksum
type Room = ([String], Int, String)

main :: IO ()
main = do
    inp <- getContents
    let rooms = filter isRealRoom . parseRooms $ inp
    putStrLn $ "Part 1: " ++ (part1 rooms)
    putStrLn $ "Part 2: " ++ (part2 rooms)

part1 :: [Room] -> String
part1 = show . sum . map sectorId
    where sectorId (_, sectorId', _) = sectorId'

part2 :: [Room] -> String
part2 = show . snd . head . filter ((== "northpole object storage") . fst) . map decodeRoom
    where decodeRoom (name, sectorId, _) = (unwords . map (map (roll sectorId)) $ name, sectorId)

roll :: Int -> Char -> Char
roll n c = Char.chr $ a + ((Char.ord c - a + n) `mod` 26)
    where a = Char.ord 'a'

isRealRoom :: Room -> Bool
isRealRoom (nameParts, sectorId, checksum) = checksum == expected
    where expected = take 5 . map fst . List.sortBy compareCountThenAlphabetical . Map.toList $ counts
          counts = foldl account Map.empty (concat nameParts)
          account counter c = Map.insertWith (+) c 1 counter
          compareCountThenAlphabetical (x1, c1) (x2, c2)
            | c1 < c2 = GT
            | c1 > c2 = LT
            | c1 == c2 = compare x1 x2

parseRooms :: String -> [Room]
parseRooms inp = map parseRoom . lines $ inp
    where parseRoom = fst . head . readP_to_S pRoom

-- Parser Tokens

pRoom :: ReadP Room
pRoom = do
    name <- sepBy1 pWord (char '-')
    char '-'
    id <- pNumber
    char '['
    checksum <- pWord
    char ']'
    return (name, id, checksum)

pNumber :: ReadP Int
pNumber = do
    number <- munch1 Char.isDigit
    return (read number)

pWord :: ReadP String
pWord = munch1 Char.isAlpha