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

  numericValue :: Hand -> Integer
  numericValue Empty = 0
  numericValue (Add card hand) = valueCard card + numericValue hand

  empty :: Hand
  empty = Empty

  value :: Hand -> Integer
  value Empty = 0
  value hand =
              if val > 21
                then  val - 10*numberOfAces hand
              else val
                where val = numericValue hand

  valueRank :: Rank -> Integer
  valueRank Jack = 10
  valueRank Queen = 10
  valueRank King = 10
  valueRank Ace = 11
  valueRank (Numeric int) = int

  valueCard ::  Card -> Integer
  valueCard card = valueRank (rank card)

  numberOfAces :: Hand -> Integer
  numberOfAces Empty = 0
  numberOfAces (Add card hand) = if(rank card == Ace)
          then 1 + numberOfAces hand
          else 0 + numberOfAces hand

  gameOver :: Hand -> Bool
  gameOver hand = if val > 21
                    then True
                  else False
                    where val = value hand

  winner :: Hand -> Hand -> Player
  winner guestHand bankHand
                      | gameOverhand == True = Bank
                      | val1 == val2 = Bank
                      | val1 > val2 = Guest
                      | otherwise = Bank
                      where gameOverhand = gameOver guestHand
                            val1 = value guestHand
                            val2 = value bankHand
