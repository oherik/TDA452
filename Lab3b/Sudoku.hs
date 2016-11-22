module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

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
isSolved sudoku = all (all (not . isNothing)) (rows sudoku)

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
        blocks' = [(concat [ take 3 (drop x row)
                  | row <- take 3 (drop y rows')])
                  | x <- [0, 3, 6], y <- [0,3,6]]

-- Property that states that for each Sudoku,
--there are 3*9 blocks and each block has exactly 9 cells
prop_SudokuBlocks :: Sudoku -> Bool
prop_SudokuBlocks sudoku = length blocks' == 27 &&
                        and [length b == 9 | b <- blocks']
  where blocks' = blocks sudoku

-- Given a Sudoku, checks that everything does not
-- contain the same digit twice
isOkay :: Sudoku -> Bool
isOkay sudoku = all isOkayBlock (blocks sudoku)

-------------------------------------------------------------------------

blanks :: Sudoku -> [Pos]
blanks _ = undefined

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) _ _ = undefined

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update _ _ _ = undefined

candidates :: Sudoku -> Pos -> [Int]
candidates _ _ = undefined

-------------------------------------------------------------------------

type Pos = (Int,Int)

solve :: Sudoku -> Maybe Sudoku
solve _ = undefined

readAndSolve :: FilePath -> IO ()
readAndSolve _ = undefined

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf _ _ = undefined

prop_SolveSound :: Sudoku -> Property
prop_SolveSound _ = undefined
