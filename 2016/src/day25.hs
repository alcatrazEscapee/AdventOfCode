{-

=== Analysis of the structure of the input ===

00 | cpy a d   | d = a
01 | cpy #X c  | c = #X
02 | cpy #Y b  | b = #Y     d += b * c  \
03 | inc d     | d++         d += b  \  |
04 | dec b     | b--                 |  |
05 | jnz b -2  | if b != 0: goto 03  /  |
06 | dec c     | c--                    |
07 | jnz c -5  | if c != 0: goto 02     /  -- d = a + #X * #Y
08 | cpy d a   | a = d
09 | jnz 0 0   | no-op
10 | cpy a b   | b = a
11 | cpy 0 a   | a = 0
12 | cpy 2 c   | c = 2                         <--+   | loop {                 \
13 | jnz b 2   | if b != 0: goto 15 --+ <-+       |   |   c = 2                | Division loop
14 | jnz 1 6   | goto 20              |   | --+   |   |   if b = 0: exit <-+   | a = b / 2
15 | dec b     | b--               <--+   |   |   |   |   b--              |   | c = b % 2
16 | dec c     | c--                      |   |   |   |   c--              |   |
17 | jnz c -4  | if c != 0: goto 13 ------+   |   |   |   if c != 0: goto--+   |
18 | inc a     | a++                          |   |   |   a++                  |
19 | jnz 1 -7  | goto 12                      | --+   | }                      /
20 | cpy 2 b   | b = 2               \     <--+
21 | jnz c 2   | if c != 0: goto 23  |
22 | jnz 1 4   | goto 26             | Since `c` will be 2 or 1, this loops 2 or 1 times to produce `b = 0` or `b = 1`
23 | dec b     | b--                 | The value of `b` is then dependent on the remainder of the above division
24 | dec c     | c--                 | It will keep dividing a / 2 until it reaches zero, at which point the program loops
25 | jnz 1 -4  | goto 21             /
26 | jnz 0 0   | no-op
27 | out b     | output <- b
28 | jnz a -19 | if a != 0: goto 9  \ Loops back to the beginning, once a reaches zero (infinite loop)
29 | jnz 1 -21 | goto 8             /

Based on this, we need to produce an output of 0, 1, 0, 1, ...
This means we have a number d = a + #X * #Y, which the bits of which are (LSB first) 010101...1
We then need the smallest such number, and since a > 0, we need the first number > #X * #Y with the form 0101010...1

-}

import Data.Bits

main :: IO ()
main = do
    inp <- getContents
    let salt = parse inp
    putStrLn $ "Part 1: " ++ (show . head . filter (> 0) . map ((+) (-salt)) $ candidates 2)

-- When n = 10 (binary), produces an infinite sequence of the binary numbers 10, 1010, 101010...
candidates :: Int -> [Int]
candidates n = n : (candidates ((n `shiftL` 2) .|. 2))

-- Parses out #X and #Y from the input and returns their product
parse :: String -> Int
parse inp = foldl (*) 1 $ map (read . head . tail . words) . take 2 . drop 1 . lines $ inp