module BlackJack where
  import Cards
  import RunGame
  import System.Random
  import Test.QuickCheck hiding (shuffle) -- To avoid conflict with our
                                          -- shuffle function

---------------------------------------
 -- A
---------------------------------------

 -- 3.2

 -- size hand2
 --    = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
 --    = 1 + size (Add (Card Jack Spades) Empty)
 --    = 1 + 1 + size Empty
 --    = 1 + 1 + 0
 --    = 1 + 1
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
                      | gameOver guestHand  = Bank
                      | gameOver bankHand = Guest
                      | val1 > val2 = Guest
                      | otherwise = Bank
                      where val1 = value guestHand
                            val2 = value bankHand

  (<+) :: Hand -> Hand -> Hand
  (<+) Empty hand = hand
  (<+) hand Empty = hand
  (<+) (Add topLast Empty) bottom = (Add topLast bottom)
  (<+) (Add topCard top) bottom = (Add topCard (top <+ bottom))

  prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
  prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

  -- Add all cards for the given suit to a list
  createFullSuit :: Suit -> [Card]
  createFullSuit suit = [(Card Ace suit)] ++
                        [(Card (Numeric a) suit) | a <- [2..10]] ++
                        [(Card b suit) | b <- [Jack, Queen, King]]

  -- Given a list, return a hand with the cards of the list
  combineCards :: [Card] -> Hand
  combineCards [card] = Add card Empty
  combineCards (card:xs) = Add card (combineCards xs)

  fullDeck :: Hand
  fullDeck = combineCards (createFullSuit Hearts ++
                          createFullSuit Spades ++
                          createFullSuit Diamonds ++
                          createFullSuit Clubs)

  draw :: Hand -> Hand -> (Hand,Hand)
  draw Empty _ = error "draw: The deck is empty"
  draw (Add topDeckCard restOfDeck) hand =
          (restOfDeck, (Add topDeckCard hand))

  -- Plays the bank, starting with an empty hand
  playBank :: Hand -> Hand
  playBank deck = playBank' deck Empty

  -- The bank draws from the deck until its hand value is 16 or above
  playBank' :: Hand -> Hand -> Hand
  playBank' deck bankHand | value bankHand < 16 =
                                    uncurry playBank' (draw deck bankHand)
                       | otherwise = bankHand

  -- Given a random generator and a hand, shuffle the cards
  -- return the shuffled hand
  shuffle :: StdGen -> Hand -> Hand
  shuffle g Empty = Empty
  shuffle g fromHand = Add card (shuffle g' hand)
      where
            (cardIndex, g') = randomR (0, size fromHand - 1) g
            (card, hand) = removeCard fromHand cardIndex

  --Removes the n:th card from a deck
  removeCard :: Hand -> Integer -> (Card, Hand)
  removeCard _ n | n<0 = error "removeCard: negative index"
  removeCard hand n = removeCard' Empty hand n

  -- topPart is reversed in this manner, making it a stack. This means we
  -- need the addReverse function, instead of the previously created (<+)
  removeCard' :: Hand -> Hand -> Integer -> (Card, Hand)
  removeCard' topPart Empty n = error "removeCard: index exceeds hand size"
  removeCard' topPart (Add c bottomPart) 0 =
                        (c, topPart `addReverse` bottomPart)
  removeCard' topPart (Add c bottomPart) n =
                        removeCard' (Add c topPart) bottomPart (n-1)

  -- Adds one hand on top of the other in reverse order
  addReverse :: Hand -> Hand -> Hand
  addReverse Empty hand = hand
  addReverse hand Empty = hand
  addReverse (Add topLast Empty) bottom = (Add topLast bottom)
  addReverse (Add topCard top) bottom = addReverse top (Add topCard bottom)

  -- Check if a given card is in the hand
  belongsTo :: Card -> Hand -> Bool
  c `belongsTo`Empty = False
  c `belongsTo`(Add c' h) = c == c' || c `belongsTo` h

  -- Check if a card is in a deck before it has been shuffled
  -- then it should be in the deck afterwards
  prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
  prop_shuffle_sameCards g c h = c `belongsTo` h ==
                                c `belongsTo` shuffle g h

  -- States that the prize is preserved
  prop_size_shuffle :: StdGen -> Hand -> Bool
  prop_size_shuffle g hand = size hand == size (shuffle g hand)

  -- 3.5
  implementation = Interface
    { iEmpty    = empty
    , iFullDeck = fullDeck
    , iValue    = value
    , iGameOver = gameOver
    , iWinner   = winner
    , iDraw     = draw
    , iPlayBank = playBank
    , iShuffle  = shuffle
  }

  main :: IO ()
  main = runGame implementation
