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
isSolved :: Sudoku -> Bool --TODO: should isSudoku be here or not?
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
   sequence_ $ map putStrLn [map maybeToChar row | row <- rows sudoku]

--TODO or use the lambda function
-- (\ c -> if isNothing c then '.' else chr $ (fromJust c) + (ord '0'))
-- instad of maybeToChar. Might look fancier, but debugging using QuickCheck
-- will be more difficult

maybeToChar :: Maybe Int -> Char
maybeToChar Nothing = '.'
maybeToChar (Just j) = chr $ j + ord '0'

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
                s <- readFile path
                let sudoku = Sudoku [map charToMaybe line | line <- lines s]
                if isSudoku sudoku
                  then return sudoku
                else error "Not a valid sudoku"

--TODO could use a lambda function here aswell

charToMaybe :: Char -> Maybe Int
charToMaybe '.' = Nothing
charToMaybe x  = Just $ ord x - ord '0'

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(1, rJust),(9,  return Nothing)]

rJust :: Gen (Maybe Int)
rJust = elements [Just j | j <- [1..9]]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
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


--
--
-- blocks :: Sudoku -> [Block]
--
-- isOkay :: Sudoku -> Bool
