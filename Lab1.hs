-- Part 1 
-- The number of steps is recursive: #(k) = 1 + #(k-1), and #(0)=1. #(k) is 
-- heri the number of steps used in a power n^k. It's therefore linear in k
-- with k+1 steps being used.

-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n k = product (replicate k n)

-- Part 3
-- This function only allows positive integers as input
power2 :: Integer -> Integer -> Integer
power2 n k =
	if(k==0)
		then 1
		else if(even k)
			then power2 (n*n) (div k 2) -- uses div to accept Integrals
			else n*(power2 n (k-1))

-- Part 4
-- We need to check
-- 	Base cases. These are n = 0, and k = 0. This shows basic functionality
--	Both odd and even k, to ensure that the recursion in power2 works.
-- 	Cases where n<0 or k<0, to make sure errors are thrown.
--	Cases where n or k aren't integers, to make sure errors are thrown. 

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

prop_powers n k = (power n k == power1 n k) == power2 n k

