module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving (Eq)

-- Custom function to display a sudoku the way it's printed in
-- the lab assignment.
instance Show Sudoku where
  show (Sudoku rows') = unlines [map (\ c -> if isNothing c then '.'
                                        else chr $ (fromJust c) +
                                        (ord '0'))row | row <- rows']

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku(replicate 9 (replicate 9 Nothing))

-- Checks that a sudoku consists of a 9x9 matrix, and that all values in
-- the matrix are valid
isSudoku :: Sudoku -> Bool
isSudoku sudoku = and ([length rows' == 9, (all (==9) . map length) rows'] ++
                      [0<j && j<10 | (Just j) <- concat rows'])
  where
      rows' = rows sudoku

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sudoku = all (all (not . isNothing)) $ rows sudoku

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = do
   sequence_ $ map putStrLn [map
    (\ c -> if isNothing c then '.' else chr $ (fromJust c) + (ord '0'))
    row | row <- rows sudoku]

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
                s <- readFile path
                let sudoku = Sudoku [map
                      (\ c -> if c == '.' then Nothing
                        else Just $ ord c - ord '0')
                      line | line <- lines s]
                if isSudoku sudoku
                  then return sudoku
                else error "Not a valid sudoku"

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(1, rJust),(9, return Nothing)]
  where rJust = elements [Just j | j <- [1..9]]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9]] | i <- [1..9]]
       return (Sudoku rows)

-- Checks if the generated sudoku is a real sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku

-------------------------------------------------------------------------

type Block = [Maybe Int]

-- Checks if the given block does not contain the same digit twice
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (Nothing:xs) = isOkayBlock xs
isOkayBlock (x:xs) = if elem x xs then False
                                  else isOkayBlock xs

-- Given a Sudoku, creates a list of all blocks of that Sudoku
blocks :: Sudoku -> [Block]
blocks sudoku = rows' ++ columns' ++ blocks'
  where rows' = (rows sudoku)
        columns' = transpose rows'
        blocks' = [ square x y | x <- [0..2], y <- [0..2] ]
        square x y = concat [ take 3 (drop (3*y) row) | row <- take 3 (drop (3*x) rows') ]

-- Property that states that for each Sudoku,
--there are 3*9 blocks and each block has exactly 9 cells
prop_SudokuBlocks :: Sudoku -> Bool
prop_SudokuBlocks sudoku = length blocks' == 27 &&
                        and [length b == 9 | b <- blocks']
  where blocks' = blocks sudoku

-- Given a Sudoku, checks that everything does not
-- contain the same digit twice
isOkay :: Sudoku -> Bool
isOkay sudoku = all isOkayBlock $ blocks sudoku

-------------------------------------------------------------------------

type Pos = (Int,Int)

-- A position generator, creating a position between (0,0) and (8,8).
-- Used for quickcheck testing
rPos :: Gen Pos
rPos = do i <- elements [0..8]
          j <- elements [0..8]
          return (i,j)

-- Given a Sudoku returns a list of the positions of the blanks elements
blanks :: Sudoku -> [Pos]
blanks sudoku =  [(i,j) | i <- [0..8], j<- [0..8],
                    isNothing ((rows sudoku !! i) !! j)]

-- Proprtery that states that all cells in the blanks list are blanks
prop_blanks :: Sudoku -> Bool
prop_blanks sudoku = all (\element -> isNothing (((rows sudoku) !!
                        fst element) !! snd element)) (blanks sudoku)

-- Given a list and a tuple and a new value.
-- Updates the given list with the new value at the given index.
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (n,_) | n < 0 || n > (length xs - 1) =
                        error "Index out of bounds"
(!!=) xs (n,x) | otherwise = h ++ x:(drop 1 t)
  where (h,t) = splitAt n xs

-- Check that the length is the same, and that all values other than
-- the chosen one is unaffected. The chosen one should be changed,
prop_replace :: [Integer] -> (Int,Integer) -> Bool
prop_replace xs (n,x) = length xs == length rep &&
                and [xs !! i == (rep !! i) |
                      i<-[0.. (length xs -1)], not (i == n)] &&
                rep !! n == x
                where
                  rep = xs !!= (n,x)

-- Given a Sudoku, a position and a new cell value.
--Update the given Sudoku with the new value in the given position.
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sudoku (i,j) val = Sudoku $ rows' !!= (i,updated)
  where
    rows' = rows sudoku
    updated = rows' !! i !!= (j,val)

-- Property that checks that the updated
--position really has gotten the new value
prop_update :: Sudoku -> Maybe Int -> Property
prop_update sudoku val = forAll rPos (\ pos -> prop_update' pos sudoku val)
  where
    prop_update' :: Pos -> Sudoku -> Maybe Int -> Bool
    prop_update' (i,j) sudoku val = isSudoku sudoku &&
                          and [rows' !! n == (new !! n)
                              | n <- [0..8], not (n==i)] &&
                          and [rows' !! i !! m == (new !! i !! m)
                              | m <- [0..8], not (m==j)] &&
                          new !! i !! j == val
      where
        rows' = rows sudoku
        new = rows $ update sudoku (i,j) val


-- isOkPos checks if the change is valid for a certain position, chiecking its
-- row, column and 3x3 block. This saves time compared to running the isOkay
-- on the whole sudoku
candidates  :: Sudoku -> Pos -> [Int]
candidates sudoku (i,j)  = [1..9] \\ catMaybes (row ++ col ++ block)
  where
    blocks' = blocks sudoku
    row = blocks' !! i
    col = blocks' !! (j+9)
    block = blocks' !! (i `div` 3 * 3 + j `div` 3 + 18)

-- Makes sure that all candidates given by an "okay" sudoku are valid
prop_candidates :: Sudoku -> Property
prop_candidates sudoku = isOkay sudoku ==>
                        forAll rPos (\ pos -> prop_candidates' pos sudoku)
  where
    prop_candidates' :: Pos -> Sudoku -> Bool
    prop_candidates' (i,j) sudoku = all isOkay [update sudoku (i,j)
                                        (Just v) | v <-
                                        candidates sudoku (i,j)]

-- Recursively solves a sudoku
solve :: Sudoku -> Maybe Sudoku
solve sudoku = if isSudoku sudoku && isOkay sudoku then
                  solve' sudoku (blanks sudoku)
              else Nothing
  where
    solve' :: Sudoku -> [Pos] -> Maybe Sudoku
    solve' sudoku [] = Just sudoku
    solve' sudoku (blank:bs) = listToMaybe $ catMaybes $
                                map (\ s -> solve' s bs)
                                [(update sudoku blank (Just x))
                                | x <- candidates sudoku blank]

-- reading the Sudoku from the given file, solve it and print the answer
readAndSolve :: FilePath -> IO ()
readAndSolve path = do
                    sud <- readSudoku path
                    let solvedSudoku = solve sud
                    if isNothing solvedSudoku
                      then error "(no solution)"
                    else printSudoku (fromJust solvedSudoku)

-- Given two Sudokus, checks if the first one is a solution
-- and if the first one is a solution of the second one
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solution subject = isSudoku solution &&
                                isSudoku subject &&
                                isSolved solution &&
                                isOkay solution && and
                                (zipWith (\a b -> a == b || isNothing b) 
                                s1 s2)
 where
   s1 = concat $ rows solution
   s2 = concat $ rows subject

-- Property that states if the function solve is sound.
-- Soundness means that the output from solve is a valid solution.
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sudoku = isSudoku sudoku &&
                          not (isNothing (solve sudoku)) ==>
                          (fromJust (solve sudoku) `isSolutionOf` sudoku)
