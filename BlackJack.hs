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
  value Empty = 0
  value (Add card hand) | valueCard card + value hand > 21 =
      valueCard card + value hand - 10*numberOfAces hand
                        | otherwise  =
        valueCard card + value hand
  --  Integer noAces = numberOfAces hand
  --  if value > 21 && noAces > 0
  --    then value - 10*noAces
  --  else
  --    then value

  valueRank :: Rank -> Integer
  valueRank Jack = 10
  valueRank Queen = 10
  valueRank King = 10
  valueRank Ace = 11
  valueRank x = x

  valueCard ::  Card -> Integer
  valueCard card = valueRank (rank card)

  numberOfAces :: Hand -> Integer
  numberOfAces Empty = 0
  numberOfAces (Add card hand) = if(rank card == Ace)
          then 1 + numberOfAces hand
          else then 0 + numberOfAces hand

  -- gameOver :: Hand -> Bool

  -- winner :: Hand -> Hand -> Player
