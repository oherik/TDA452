-- Part 1 
-- The number of steps is recursive: #(k) = 1 + #(k-1), and #(0)=1. #(k) is 
-- heri the number of steps used in a power n^k. It's therefore linear in k
-- with k+1 steps being used.

-- Part 2
power1 n k = product (replicate k n)
