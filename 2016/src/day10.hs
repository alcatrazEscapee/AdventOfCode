import qualified Data.Char as Char
import qualified Data.Map as Map

import Text.ParserCombinators.ReadP
import Data.Maybe

type IntMultiMap = Map.Map Int [Int]
type ValueAndOutputMap = (IntMultiMap, IntMultiMap)
type ValueAndOutputMapAndBot = (IntMultiMap, IntMultiMap, Int)

data Instruction = 
    Value Int Int | -- Bot, Value
    Split Int Bool Int Bool Int -- Bot, LowIsOutput, Low, HighIsOutput, High
    deriving (Show)

isValue :: Instruction -> Bool
isValue (Value _ _) = True
isValue _ = False

main :: IO ()
main = do
    inp <- getContents
    let (part1, part2) = solve inp
    putStrLn $ "Part 1: " ++ part1
    putStrLn $ "Part 2: " ++ part2

solve :: String -> (String, String)
solve inp = (part1, part2)
    where part1 = show resultBot
          part2 = show $ output 0 * output 1 * output 2
                where output x = head . fromJust . Map.lookup x $ resultOutputs

          (resultOutputs, resultBot) = simulate values Map.empty (-1) :: (IntMultiMap, Int)
          
          -- Recursive simulation
          -- State is the values and outputs multimap, along with the single int identifying the target bot for part 1
          simulate :: IntMultiMap -> IntMultiMap -> Int -> (IntMultiMap, Int)
          simulate vs os bot61And17 = if null v2s
                                      then (os, bot61And17) -- No bots have two values, so return outputs
                                      else simulate vs' os' bot61And17'
                where v2s = botsWithTwoValues vs
                      (vs', os', bot61And17') = splitAllBotValues v2s (vs, os, bot61And17)

          -- Applies splitBotValues to the list of bots with two values, until the list is empty
          splitAllBotValues :: [(Int, [Int])] -> ValueAndOutputMapAndBot -> ValueAndOutputMapAndBot
          splitAllBotValues [] x = x
          splitAllBotValues (v2:v2s) (vs, os, bot61And17) = splitAllBotValues v2s (splitBotValues v2 (vs, os, bot61And17))
        
          -- Finds the rule matching the given bot, which has the values v1, v2
          -- Applies the rule to the two given values
          splitBotValues :: (Int, [Int]) -> ValueAndOutputMapAndBot -> ValueAndOutputMapAndBot
          splitBotValues (bot, (v1:v2:_)) (vs, os, bot61And17) = (vs3, os2, bot61And17')
                where os1 = if lowIsOutput then Map.insertWith (++) low [vLow] os else os
                      os2 = if highIsOutput then Map.insertWith (++) high [vHigh] os1 else os1
                    
                      vs1 = if lowIsOutput then vs else Map.insertWith (++) low [vLow] vs
                      vs2 = if highIsOutput then vs1 else Map.insertWith (++) high [vHigh] vs1
                      vs3 = Map.delete bot vs2

                      bot61And17' = if vLow == 17 && vHigh == 61 then bot else bot61And17 :: Int

                      vLow = if v1 < v2 then v1 else v2 :: Int
                      vHigh = if v1 < v2 then v2 else v1 :: Int

                      (Split _ lowIsOutput low highIsOutput high) = head . filter (matchRule bot) $ rules
                            where matchRule bot' (Split botToTest _ _ _ _) = bot' == botToTest
                                  matchRule _ _ = error "Not a Split"

          splitBotValues _ _ = error "Invalid Input"

          botsWithTwoValues :: IntMultiMap -> [(Int, [Int])]
          botsWithTwoValues vs = Map.toList . Map.filter ((> 1) . length) $ vs

          values = Map.fromListWith (\a b -> a ++ b) . map toMapValue . filter isValue $ instructions :: Map.Map Int [Int]
                where toMapValue (Value bot value) = (bot, [value])
                      toMapValue _ = error "Not a Value"
          
          rules = filter (not . isValue) $ instructions
          instructions = parse inp :: [Instruction]

          parse :: String -> [Instruction]
          parse = map parseLine . lines
          parseLine = fst . head . readP_to_S pInstruction


-- Parser Tokens

pInstruction :: ReadP Instruction
pInstruction = do
    name <- pWord
    char ' '
    inst <- case name of 
        "value" -> pValueInstruction
        "bot" -> pSplitInstruction
    return inst

pValueInstruction :: ReadP Instruction
pValueInstruction = do
    value <- pInt
    string " goes to bot "
    bot <- pInt
    return (Value bot value)

pSplitInstruction :: ReadP Instruction
pSplitInstruction = do
    bot <- pInt
    string " gives low to "
    lowType <- pWord
    char ' '
    low <- pInt
    string " and high to "
    highType <- pWord
    char ' '
    high <- pInt
    return (Split bot (lowType == "output") low (highType == "output") high)

pWord :: ReadP String
pWord = munch1 Char.isAlpha

pInt :: ReadP Int
pInt = do
    v <- munch1 Char.isDigit
    return (read v)