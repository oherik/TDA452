import Test.QuickCheck

-- From the lecture
power n 0	= 1
power n k	| k > 0 = power n (k-1)*n
-- power n k	| k < 0 = 1 `div` power n (-k)

-- Part 1 
-- The number of steps is recursive: #(k) = 1 + #(k-1), and #(0)=1. #(k) is 
-- here the number of steps used in a power n^k. It's therefore linear in k
-- with k+1 steps being used.

-- Part 2
power1 n k 	| k < 0 	= error "power1: negative argument"
			|otherwise 	= product (replicate k n)

-- Part 3
-- This function only allows integers as input
power2 n 0 	= 1
power2 n k 	| k < 0 	= error "power2: negative argument"  
			| even k 	= power2 (n*n) (div k 2) -- uses div to accept Integrals
			| otherwise =  n*(power2 n (k-1))

-- Part 4
-- We need to check
-- 	Base cases. These are n = 0, and k = 0. This shows basic functionality
--	Both odd and even k, to ensure that the recursion in power2 works.
-- 	Cases where or k<0, to make sure errors are thrown.
--	Cases where n or k aren't integers, to make sure errors are thrown. 
prop_power n k = power n (abs k) == n ^ (abs k) && 
				power n (abs k) == power1 n (abs k) && 
				power n (abs k) == power2 n (abs k)







