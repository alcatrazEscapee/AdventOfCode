
{- 
Consider the phrase

> Disk #Nj has Mj positions; at time=0 it is at position Tj.

Assume we release at some time x. Then we must have:
    for all j; (x + Nj + Tj) ~= 0 mod Mj

Let,
    aj = -Nj - Tj
    mj = Mj

Then we can apply Chinese Remainder Theorem (CRT):

Where x = aj mod mj for j in {1, ...N}
A solution exists if m1, ... mN are coprime, or if g = gcd(m1, ...mN), a1 == ... aN mod g

Note that all the Mj in the problem description are primes, thus coprime, thus a solution does exist.
We can then just solve this with pairwise CRT, reducing until we find a final solution.
-}

main :: IO ()
main = do
    inp <- getContents
    let disks = parse inp
    putStrLn $ "Part 1: " ++ (solve disks)
    putStrLn $ "Part 2: " ++ (solve $ (-(length disks) - 1, 11) : disks)

solve :: [(Int, Int)] -> String
solve = show . fst . foldl1 crt

parse :: String -> [(Int, Int)]
parse inp = map parseLine . map words . lines $ inp
    where parseLine (_:nj:_:mj:_:_:_:_:_:_:_:tj:_) = (- (int . tail $ nj) - (int . init $ tj), int mj)
          parseLine _ = error "Invalid input line"

int :: String -> Int
int s = read s :: Int

-- Chinese Remainder Theorem
-- For a1, m1, a2, m2; finds x where x = a1 mod m1 = a2 mod m2
-- Returns (x, m), where m is the modulus s.t. any y ~= x mod m will also solve the above system
crt :: (Int, Int) -> (Int, Int) -> (Int, Int)
crt (a1, m1) (a2, m2) = (ar `mod` mr, mr)
    where (g, x, y) = egcd m1 m2
          ar = (a2 * x * m1 + a1 * y * m2) `div` g
          mr = (m1 * m2) `div` g


-- Extended Euclidean Algorithm for finding the gcd (Greatest Common Divisor)
-- For any a, b, returns gcd(a, b), x, y; s.t. a*x + b*y = gcd(a, b)
egcd :: Int -> Int -> (Int, Int, Int)
egcd 0 b = (b, 0, 1)
egcd a b = (g, y - (b `div` a) * x, x)
    where (g, x, y) = egcd (b `mod` a) a