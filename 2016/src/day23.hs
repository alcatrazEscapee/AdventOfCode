{-

=== Analysis of the structure of the input ===

Initialize a = 7 (part 1) and 12 (part 2)
#X and #Y are input dependent

00 | cpy a b   | b = a                       -- Initialize b = a - 1
01 | dec b     | b--          b = a - 1      
02 | cpy a d   | d = a        d = a          \ Multiplication Loop
03 | cpy 0 a   | a = 0                       | a = b * a
04 | cpy b c   | c = b        a += b * d  \  | b--
05 | inc a     | a++           a += b  \  |  | c = 0
06 | dec c     | c--                   |  |  | d = 0
07 | jnz c -2  | if c != 0: goto 05    /  |  |
08 | dec d     | d--                      |  |
09 | jnz d -5  | if d != 0: goto 04       /  /
10 | dec b     | b--                                                       <--+
11 | cpy b c   | c = b                                                        |
12 | cpy c d   | d = c                            / Each time we reach this, `b--`, thus `tgl c` decreases by 2
13 | dec d     | d--         c += d  \            | In part 1, `c` yields 10, 8, ... 2, in part 2, `c` yields 20, 18, ... 2
14 | inc c     | c++                 | c = 2 * b  | Once `c = 2`, the `jnz 1 c` at [18] is toggled, and we stop looping back to [2]
15 | jnz d -2  | if d != 0: goto 13  /            | The below program is then toggled at 8, 6, 4, and 2
16 | tgl c     |                                  \ The value in `a` to start will be dependent on the above loop
17 | cpy -16 c | c = -16
18 | jnz 1 c   | goto 02     toggled | cpy 1 c   | c = 1
19 | cpy #X c  |                     | cpy #X c  | c = #X
20 | jnz #Y d  |      ->     toggled | cpy #Y d  | d = #Y    a += c * d   \
21 | inc a     |                     | inc a     | a++         a += d  \  | Multiplication loop
22 | inc d     |      ->     toggled | dec d     | d--                 |  | a += #X * #Y
23 | jnz d -2  |                     | jnz d -2  | if d != 0: goto 21  /  |
24 | inc c     |      ->     toggled | dec c     | c--                    |
25 | jnz c -5  |                     | jnz c -5  | if c != 0: goto 20     /  --> Program exits

After restructuring the program based on the above input, we get

b = a
while b > 1 {
    b -= 1
    a = b * a
}
a += #X * #Y

... which is effectively computing a! + #X * #Y

-}


main :: IO ()
main = do
    inp <- getContents
    let salt = parse inp
    putStrLn $ "Part 1: " ++ show (salt + factorial 7)
    putStrLn $ "Part 2: " ++ show (salt + factorial 12)

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- Parses out #X and #Y from the input and returns their product
parse :: String -> Int
parse inp = foldl (*) 1 $ map (read . head . tail . words) . take 2 . drop 19 . lines $ inp