import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Char as Char

import Text.ParserCombinators.ReadP
import Data.Maybe

type ChipAndGenerator = (Int, Int)
type State = (Int, Map.Map ChipAndGenerator Int)

-- Up and Down movements
data Move = Up | Down

toInt :: Move -> Int
toInt Up = 1
toInt Down = -1

-- An item in the input
data Item = Generator String | Microchip String
    deriving (Show, Eq)

typeOf :: Item -> String
typeOf (Generator x) = x
typeOf (Microchip x) = x


main :: IO ()
main = do
    inp <- getContents
    let state = parse inp
    putStrLn $ "Part 1: " ++ solve state
    putStrLn $ "Part 2: " ++ solve (Map.insertWith (+) (1, 1) 2 state)

-- Invokes the BFS and returns just the answer
solve :: Map.Map ChipAndGenerator Int -> String
solve state = show $ bfs (Seq.fromList [((1, state), 0)]) Set.empty

{-
Breadth-First Search

The key realizations here are pruning the known state space
For that, we represent the state not as a series of items on floors, but as a set of pairs of floors on which paired items reside
For example, the two states:

Floor 2 Ac Ag    | Floor 2 Bc Bg
Floor 1 Bc Bg    | Floor 1 Ac Ag

Both are represented by the set {(2, 2), (1, 1)}
However, this has a problem when we have multiple identical types on the same floor, for example:

Floor 1 Ac Ag Bc Bg

As we cannot represent this with the set {(2, 2), (2, 2)}
So, we instead store a map of pairs to their multiplicity: this would then be {(2, 2): 2},
and the example above would be {(1, 1): 1, (2, 2): 1}

A single state is then a pair of (elevator floor, {(floor of chip, floor of generator)})

> If a chip is ever left in the same area as another RTG, and it's not connected to its own RTG, the chip will be *fried*

In order to handle invalid states
- chips := Filter the states by chips that are on a different floor of their generator, and collect these floors
- generators := All floors with generators
- the state is invalid of these two lists of floors have any intersection

-}
bfs :: Seq.Seq (State, Int) -> Set.Set State -> Int
bfs queue seen
    -- If we reach the end of our queue without finding the target state, that's an error
    | Seq.null queue       = error "BFS terminated without reaching the target state"
    -- The terminal condition is if we reach the target state
    -- Then, return the number of steps it took to reach it
    | isTarget prev        = prevDist
    -- Avoid this state if we've seen it before!
    -- This is crucial to avoid our state space exploding.
    -- In this case, invoke the BFS with the tail of the queue
    | Set.member prev seen = bfs queueTail seen
    -- Otherwise, step to neighbors and recurse
    | otherwise            = bfs queue' seen'
    where ((prev, prevDist), queueTail) = let (h Seq.:< t) = Seq.viewl queue in (h, t)
        
          queue' = queueTail Seq.>< (Seq.fromList . map (\s -> (s, prevDist + 1)) $ neighborStates)
          seen' = Set.insert prev seen

          -- The list of all possible neighbor states
          -- These are generated from all neighbor states consisting of single and double moves, up and down, and filtered based on which ones are valid
          neighborStates :: [State]
          neighborStates = filter isValid . concat $ [moves2Up, moves1Down, moves1Up, moves2Down]
                where -- Lists of each possible movement states of a given (Up/Down, 1/2) selection
                      -- The 2x movements are done via taking the 1x movement state, then reverting the elevator,
                      -- and flat mapping (concat . map f) another set of possible 1x movements
                      moves1Up = applyMoves Up prev
                      moves2Up = concat . map (applyMoves Up . moveElevator Down) $ moves1Up
                      moves1Down = applyMoves Down prev
                      moves2Down = concat . map (applyMoves Down . moveElevator Up) $ moves1Down

                      -- Applies a given move (Up/Down) to a state, and generates the flat list of all possible states
                      -- Iterates the unique (chip, generator) pairs in the current state and tries to move each one
                      applyMoves :: Move -> State -> [State]
                      applyMoves v state@(_, m) = catMaybes . concat . map (moveAny v state) . Map.keys $ m

                      -- Move either a chip or a generator, return up to two new states given a state and particular (chip, generator) pair
                      moveAny :: Move -> State -> ChipAndGenerator -> [Maybe State]
                      moveAny v state pair = [moveChip v pair state, moveGenerator v pair state]

                      moveElevator :: Move -> State -> State
                      moveElevator v (e, s) = let dy = toInt v in ((e + dy), s)

                      moveChip :: Move -> ChipAndGenerator -> State -> Maybe State
                      moveChip v pair@(c, g) (f, m) = if c == f
                            then let dy = toInt v in Just (f + dy, mapIncDec (c + dy, g) pair m)
                            else Nothing
                    
                      moveGenerator :: Move -> ChipAndGenerator -> State -> Maybe State
                      moveGenerator v pair@(c, g) (f, m) = if g == f
                            then let dy = toInt v in Just (f + dy, mapIncDec (c, g + dy) pair m)
                            else Nothing

          -- Detect the target state as when all items are on floor 4
          isTarget :: State -> Bool
          isTarget (_, pairs) = all (\(c, g) -> c == 4 && g == 4) . Map.keys $ pairs

          -- Detect invalid states
          isValid :: State -> Bool
          isValid (elevator, pairs) = floorWithinBounds && allPairsFunctional
                where floorWithinBounds = 1 <= elevator && elevator <= 4
                      allPairsFunctional = null $ List.intersect chips generators
                    
                      chips = map (\(c, _) -> c) .filter (\(c, g) -> c /= g) . Map.keys $ pairs
                      generators = map (\(_, g) -> g) . Map.keys $ pairs

-- Useful map functions
-- Apply increment and decrement operations as if the map was a counter

mapIncDec :: Ord k => k -> k -> Map.Map k Int -> Map.Map k Int
mapIncDec inc dec = mapInc inc . mapDec dec

mapDec :: Ord k => k -> Map.Map k Int -> Map.Map k Int
mapDec key = Map.update (\v -> if v == 1 then Nothing else Just (v - 1)) key

mapInc :: Ord k => k -> Map.Map k Int -> Map.Map k Int
mapInc key = Map.insertWith (+) key 1

-- Input Parsing

parse :: String -> Map.Map ChipAndGenerator Int
parse inp = foldl (flip mapInc) Map.empty itemPairs
    where itemPairs = map (\t -> (chipOfType t itemFloorPairs, generatorOfType t itemFloorPairs)) $ itemTypes
        
          itemTypes = List.nub . map typeOf . concat $ parseResults
          itemFloorPairs = concat . map (\(f, items) -> map (\i -> (f, i)) items) . zip [1..] $ parseResults
        
          parseResults = map parseLine . lines $ inp
          parseLine = fst . head . readP_to_S pLine

          chipOfType :: String -> [(Int, Item)] -> Int
          chipOfType t items = head . map fst . filter (\(_, c) -> c == (Microchip t)) $ items

          generatorOfType :: String -> [(Int, Item)] -> Int
          generatorOfType t items = head . map fst . filter (\(_, c) -> c == (Generator t)) $ items

-- Parser Tokens

pLine :: ReadP [Item]
pLine = do
    _ <- string "The "
    _ <- pWord
    _ <- string " floor contains "
    w <- pWord
    pairs <- case w of
        "nothing" -> do
            _ <- string " relevant."
            return []
        "a" -> do
            _ <- char ' '
            many1 pItem
        _ -> error "Invalid input"
    eof
    return pairs

pItem :: ReadP Item
pItem = do
    element <- pWord
    k <- pChar
    item <- case k of
        ' ' -> do
            _ <- string "generator"
            return (Generator element)
        '-' -> do
            _ <- string "compatible microchip"
            return (Microchip element)
        _   -> error "Invalid input"
    _ <- string " and a "
        <++ string ", a "
        <++ string ", and a "
        <++ string "."
    return item

pWord :: ReadP String
pWord = munch1 Char.isAlpha

pChar :: ReadP Char
pChar = satisfy (\_ -> True)