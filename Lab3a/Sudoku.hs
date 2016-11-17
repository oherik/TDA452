module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char

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
isSolved sudoku = isSudoku sudoku && all (all (not . isNothing)) (rows sudoku)

-- example sudoku (TODO: remove!)
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just


-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = do
   sequence_ (map putStrLn [map maybeToChar row | row <- (rows sudoku)])

maybeToChar :: (Maybe Int) -> Char
maybeToChar Nothing = '.'
maybeToChar (Just j) = chr (j+ (ord '0'))

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku = undefined

charToMaybe :: Char -> Maybe Int
charToMaybe '.' = Nothing
charToMaybe x | ((ord x - ord '0') > 9) || ((ord x - ord '0') < 1 ) = error "Not a valid char"
charToMaybe x | otherwise = Just (ord x - ord '0')
 -- TODO: add a where-clause instead of (ord x - ord '0'), this was a hotfix
  -- (but I can't fix it right now since the plane is departing)
-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-------------------------------------------------------------------------
