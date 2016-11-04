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

-- Calculate the value of a hand, counting all aces as 11
  numericValue :: Hand -> Integer
  numericValue Empty = 0
  numericValue (Add card hand) = valueCard card + numericValue hand

-- Calculate the value of a hand, setting the value of all aces to 1 if
-- the sum would otherwise be >21.
  value :: Hand -> Integer
  value Empty = 0
  value hand =  if val > 21
                	then  val - 10*numberOfAces hand
              	else val
                where val = numericValue hand

--  Calculate the value of a rank.Ace is here counted as 11, as it is the
--  default value
  valueRank :: Rank -> Integer
  valueRank Jack = 10
  valueRank Queen = 10
  valueRank King = 10
  valueRank Ace = 11
  valueRank (Numeric int) = int

-- Calculate the value of a card
  valueCard ::  Card -> Integer
  valueCard card = valueRank (rank card)

-- Returns the number of aces in any given hand
  numberOfAces :: Hand -> Integer
  numberOfAces Empty = 0
  numberOfAces (Add card hand) = if(rank card == Ace)
          then 1 + numberOfAces hand
          else 0 + numberOfAces hand

-- Specifies if a hand has gone bust
  gameOver :: Hand -> Bool
  gameOver hand = if val > 21
                    then True
                  else False
                  where val = value hand

-- Determines which of two hands is the winner of the game
  winner :: Hand -> Hand -> Player
  winner guestHand bankHand
                      | gameOverhand == True = Bank
                      | val1 == val2 = Bank
                      | val1 > val2 = Guest
                      | otherwise = Bank
                      where gameOverhand = gameOver guestHand
                            val1 = value guestHand
                            val2 = value bankHand
