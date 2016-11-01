-- Part 1

-- The number of computing steps is k+1. Since it is a recursive call, we will be adding one step until k reaches 0. When k reaches 0, then we will need to add one final step.

-- Part 2

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"

power1 n k = product (replicate (fromIntegral k) n)

-- Part 3

power2:: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"

power2 n 0 = 1
power2 n k | even k = power2 (n*n) (k `div` 2)
power2 n k | odd k = n * power2 n (k-1)

-- Part 4
-- A
-- We would like to test with integers since the function should not be defined for floats. We will not test when k is less than 0, since those are not valid numbers.
--
-- The base cases we have chosen is k=0 and n=0. For negative n we want to check it with odd and even k's, to make sure that the result is either negative or positive. We also want to test positive n's with odd and even k's.
--
-- Cases:
-- n=2 k=0
-- n=0 k=2
-- n=-2 k=2
-- n=-2 k=3
-- n=2 k=2
-- n=2 k=3

-- B
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k == power1 n k) &&
                (power1 n k == power2 n k) &&
                (power2 n k == n ^ k)

-- C
power_tests = prop_powers 2 0 &&
              prop_powers 0 2 &&
              prop_powers (-2) 2 &&
              prop_powers (-2) 3 &&
              prop_powers 2 2 &&
              prop_powers 2 3

-- D
