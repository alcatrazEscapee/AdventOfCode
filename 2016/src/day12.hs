
main :: IO ()
main = do
    inp <- getContents
    putStrLn (parse inp)


-- Rather than compile and execute this assembly, or decompile it for meaning, we transpile it to C.
-- With x86-64 clang 15.0.0 -O3, this is able to reduce to a single `mov` instruction.
parse :: String -> String
parse inp = "#include <stdio.h>\n\n\
            \inline static void part1() {\n\
            \    int a, b, c, d;\n\
            \    a = b = c = d = 0;\n\
            \    d = 0;\n" ++ body ++ "\n\n    printf(\"Part 1: %d\\n\", a);\n\
            \}\n\
            \inline static void part2() {\n\
            \    int a, b, c, d;\n\
            \    a = b = c = d = 0;\n\
            \    c = 1;\n" ++ body ++ "\n\n    printf(\"Part 2: %d\\n\", a);\n\
            \}\n\
            \int main() {\n\
            \    part1();\n\
            \    part2();\n\
            \    return 0;\n\
            \}"
    where body = foldl (\acc x -> acc ++ "\n" ++ x) "" $ map parseInst . zip [0..] . map words . lines $ inp
        
          parseInst :: (Int, [String]) -> String
          parseInst (code, (op:args)) = "\tlabel_" ++ (show code) ++ ": " ++ case op of 
            "jnz" -> let (test:offset:_) = args in "if (" ++ test ++ " != 0) goto label_" ++ (show . (+) code . int $ offset) ++ ";" 
            "cpy" -> let (src:dest:_) = args in dest ++ " = " ++ src ++ ";"
            "inc" -> (head args) ++ "++;"
            "dec" -> (head args) ++ "--;"
            _ -> error "Unknown instruction " ++ op
          parseInst _ = error "Invalid instruction"

          int :: String -> Int
          int s = read s :: Int
