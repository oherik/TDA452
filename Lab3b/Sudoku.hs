module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

 -- TODO ta bort
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

type Pos = (Int,Int)

allPos :: Gen Pos
allPos =
    do  i <- choose(0,8)
        j <- choose(0,8)
        return (i,j)

-- Given a Sudoku returns a list of the positions of the blanks elements
blanks :: Sudoku -> [Pos]
blanks sudoku =  [(i,j) | i <- [0..8], j<- [0..8],
                    isNothing ((rows sudoku !! i) !! j)]

-- Proprtery that states that all cells in the blanks list are blanks
prop_blanks :: Sudoku -> Bool
prop_blanks sudoku = all (\element -> isNothing (((rows sudoku) !! fst element) !! snd element)) (blanks sudoku)


(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (n,_) | n < 0 || n > (length xs - 1) =
                        error "Index out of bounds"
(!!=) xs (n,x) | otherwise = h ++ x:(drop 1 t)
  where (h,t) = splitAt n xs

-- TODO: Haskell won't accept the name prop_!!=
-- Check that the length is the same, and that all values other than
-- the chosen one is unaffected. The chosen one should be changed,
prop_replace :: [Integer] -> (Int,Integer) -> Bool
prop_replace xs (n,x) = length xs == length rep &&
                and [xs !! i == (rep !! i) |
                      i<-[0.. (length xs -1)], not (i == n)] &&
                rep !! n == x
                where
                  rep = xs !!= (n,x)

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sudoku (i,j) val = Sudoku $ rows' !!= (i,updated)
  where
    rows' = rows sudoku
    updated = rows' !! i !!= (j,val)

prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update sudoku (i,j) val =
                      isSudoku sudoku &&
                      and [rows' !! n == (new !! n)
                          | n <- [0..8], not (n==i)] &&
                      and [rows' !! i !! m == (new !! i !! m)
                          | m <- [0..8], not (m==j)] &&
                      new !! i !! j == val
  where
    rows' = rows sudoku
    new = rows (update sudoku (i,j) val)

-- TODO fråga om detta! Baka in denna i metoden ovan, eller hur ska man
-- annars få quickCheck att bara testa rätt värden?
prop_update' :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update' sudoku (i,j) val = prop_update sudoku
                                (abs (mod i 9), abs (mod j 9)) (validJ val)
  where
    validJ (Just v) =  Just (abs (mod v 9) + 1)
    validJ Nothing = Nothing


-- isOkPos checks if the change is valid for a certain position, chicking its
-- row, column and 3x3 block. This saves time compared to running the isOkay
-- on the whole sudoku
candidates :: Sudoku -> Pos -> [Int]
candidates sudoku (i,j) = [ x | x<- [1..9], isOkPos (update sudoku (i,j) (Just x))]
  where
    isOkPos :: Sudoku -> Bool
    isOkPos sudoku' = all isOkayBlock [row, col, block]
      where
        blocks' = blocks sudoku'
        row = blocks' !! i
        col = blocks' !! (j+9)
        block = blocks' !! (i `div` 3 * 3 + j `div` 3 + 18)

candidates'  :: Sudoku -> Pos -> [Int]
candidates' sudoku (i,j)  = [1..9] \\ (catMaybes values)
  where
    blocks' = blocks sudoku
    row = blocks' !! i
    col = blocks' !! (j+9)
    block = blocks' !! (i `div` 3 * 3 + j `div` 3 + 18)
    values = row ++ col ++ block

prop_candidates :: Sudoku -> Pos -> Bool
prop_candidates sudoku pos = all isOkay [update sudoku pos (Just i)
                                | i <- candidates sudoku pos]
prop_candidates' :: Sudoku -> Property
prop_candidates' sudoku = forAll allPos (\ pos -> prop_candidates sudoku pos)

-------------------------------------------------------------------------

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
                                | x <- candidates' sudoku blank]

-- reading the Sudoku from the given file, solve it and print the answer
readAndSolve :: FilePath -> IO ()
readAndSolve path = do
                    sud <- readSudoku path
                    let solvedSudoku = solve sud
                    if isNothing solvedSudoku
                      then error "(no solution)"
                    else printSudoku (fromJust solvedSudoku)

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solution subject = isSudoku solution &&
                                isSolved solution && and
                                  [s1 !! i == (s2 !! i) ||
                                  (s2 !! i == Nothing) | i <- [0..80]]
 where
   s1 = concat $ rows solution
   s2 = concat $ rows subject
-- Idé: list comprehension. Concata alla celler. Gå igenom alla, plocka element i från båda och kolla
-- om antingen båda är samma eller om den från sudoku2 är Nothing

prop_SolveSound :: Sudoku -> Property
prop_SolveSound _ = undefined

-- Idé: testa om solve något är isSolutionOf något
