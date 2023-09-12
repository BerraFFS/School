{-
Group number: 58
Group members: Allan Khaledi, Ali Berat Can
-}

import Cards
import RunGame

aCard1 :: Card
aCard1 = Card Ace Spades -- define your favorite card here

aCard2 :: Card
aCard2 = Card (Numeric 10) Hearts -- define another card here

aHand :: Hand
aHand = [aCard1, aCard2] -- a Hand with two Cards, aCard1 and aCard2


-- We created new instances for cards to use in a new hand while testing the gameOver and winner functions

aCard3 :: Card
aCard3 = Card (Numeric 10) Diamonds 

aCard4 :: Card
aCard4 = Card (Numeric 10) Spades 

aCard5 :: Card
aCard5 = Card (Numeric 10) Clubs 

aHand2 :: Hand
aHand2 = [aCard3, aCard4, aCard5] 

-- Task 1A

{-
size hand2
  = size [Card (Numeric 2) Hearts, Card Jack Spades]
  = size ((Card (Numeric 2) Hearts), (Card Jack Spades), [])
  = 1 + size ((Card Jack Spades), [])
  = 1 + 1 + size []
  = 1 + 1 + 0
  = 2
-}

-- Task 2A

displayCard :: Card -> String
displayCard (Card r s) | r == King || r == Queen || r == Jack || r == Ace = show r  ++ " of " ++ show s
                       | otherwise = show (valueRank r)  ++ " of " ++ show s

display :: Hand -> String
display hnd = unwords (lines (unlines [displayCard x | x <- hnd]))

-- Task 3A

valueRank :: Rank -> Int
valueRank Ace         = 11
valueRank (Numeric x) = x
valueRank _ = 10

valueCard :: Card -> Int
valueCard (Card r s) = valueRank r

numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces hnd = length [Ace | Card r _ <- hnd, r == Ace]

-- A set value for the hand before any corrections that are needed with multiple aces
valueHand :: Hand -> Int
valueHand hnd = sum [valueCard x | x <- hnd]

value :: Hand -> Int
value [] = 0
value hnd | valueHand hnd > 21 = valueHand hnd - (10 * numberOfAces hnd) 
          | otherwise = valueHand hnd

-- Task 4A

gameOver :: Hand -> Bool
gameOver hnd = value hnd > 21 

winner :: Hand -> Hand -> Player
winner guestHand bankHand | gameOver guestHand                = Bank
                          | gameOver bankHand                 = Guest
                          | value guestHand > value bankHand  = Guest
                          | value guestHand < value bankHand  = Bank
                          | value guestHand == value bankHand = Bank


allSuits :: [Suit]
allSuits = [Hearts, Spades, Diamonds, Clubs]

allRank ::  [Rank]
allRank = [Ace, King, Queen, Jack, 
           Numeric 2, Numeric 3, Numeric 4, 
           Numeric 5, Numeric 6, Numeric 7, 
           Numeric 8, Numeric 9, Numeric 10]

fullDeck :: Deck
fullDeck = [Card r s | r <- allRank, s <- allSuits]

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

draw :: Deck -> Hand -> (Deck, Hand)
draw [] = error "draw: The deck is empty."
