module BlackJack where
  import Cards
  import RunGame
  import System.Random
  import Test.QuickCheck

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
  gameOver hand = value hand > 21


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

  (<+) :: Hand -> Hand -> Hand
  (<+) Empty hand = hand
  (<+) hand Empty = hand
  (<+) (Add topLast Empty) bottom = (Add topLast bottom)
  (<+) (Add topCard top) bottom = (Add topCard (top <+ bottom))

  prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
  prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

  draw :: Hand -> Hand -> (Hand,Hand)
  draw Empty _ = error "draw: The deck is empty"
  draw (Add topDeckCard restOfDeck) hand = (restOfDeck, (Add topDeckCard hand))

  -- Plays the bank, starting with an empty hand
  playBank :: Hand -> Hand
  playBank deck = playBank' deck Empty

  -- The bank draws from the deck until its hand value is 16 or above
  playBank' :: Hand -> Hand -> Hand
  playBank' deck bankHand | value bankHand < 16 = playBank' deck1' bankHand1'
                       | otherwise = bankHand
                       where (deck1',bankHand1') = draw deck bankHand

  --shuffle :: StdGen -> Hand -> Hand

  --Removes the n:th card from a deck
  removeCard :: Hand -> Integer -> (Card, Hand)
  removeCard _ n | n<0 = error "removeCard: negative index"
  removeCard hand n = removeCard' Empty hand n

  -- topPart is reversed in this manner, making it a stack. This means we
  -- need the addReverse function, instead of the previously created (<+)
  removeCard' :: Hand -> Hand -> Integer -> (Card, Hand)
  removeCard' topPart Empty n = error "removeCard: index exceeds hand size"
  removeCard' topPart (Add c bottomPart) 0 = (c, topPart `addReverse`
                                            bottomPart)
  removeCard' topPart (Add c bottomPart) n = removeCard' (Add c topPart)
                                            bottomPart (n-1)

  -- Adds one hand on top of the other in reverse order
  addReverse :: Hand -> Hand -> Hand
  addReverse Empty hand = hand
  addReverse hand Empty = hand
  addReverse (Add topLast Empty) bottom = (Add topLast bottom)
  addReverse (Add topCard top) bottom = addReverse top (Add topCard bottom)
