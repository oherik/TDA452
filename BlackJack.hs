module BlackJack where
  import Cards
  import RunGame

---------------------------------------
 -- A
---------------------------------------

 -- 3.2

-- size hand2
--    = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
--    = 1 + size (Add (Card Jack Spades) Empty)
--    = 1 + 1 + size Empty
--    = 2 + size Empty
--    = 2 + 0
--    = 2

  -- 3.3

  empty :: Hand
  empty = Empty

  value :: Hand -> Integer

  valueRank :: Rank -> Integer

  valueCard ::  Card -> Integer

  numberOfAces :: Hand -> Integer

  gameOver :: Hand -> Bool

  winner :: Hand -> Hand -> Player
