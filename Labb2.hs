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

handEmpty :: Hand
handEmpty = []

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
display hand = unwords (lines (unlines [displayCard x | x <- hand]))

-- Task 3A

valueRank :: Rank -> Int
valueRank Ace         = 11
valueRank (Numeric x) = x
valueRank _ = 10

valueCard :: Card -> Int
valueCard (Card r s) = valueRank r

numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces hand = length [Ace | Card r _ <- hand, r == Ace]

-- A set value for the hand before any corrections that are needed with multiple aces
valueHand :: Hand -> Int
valueHand hand = sum [valueCard x | x <- hand]

value :: Hand -> Int
value [] = 0
value hand | valueHand hand > 21 = valueHand hand - (10 * numberOfAces hand) 
           | otherwise = valueHand hand

-- Task 4A

gameOver :: Hand -> Bool
gameOver hand = value hand > 21 

winner :: Hand -> Hand -> Player
winner guestHand bankHand | gameOver guestHand                = Bank
                          | gameOver bankHand                 = Guest
                          | value guestHand > value bankHand  = Guest
                          | value guestHand < value bankHand  = Bank
                          | value guestHand == value bankHand = Bank

-- Task 1B
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

-- Task 2B
draw :: Deck -> Hand -> (Deck, Hand)
draw [] hand = error "draw: The deck is empty."
draw deck hand = (tail deck, head deck : hand) 

-- Task 3B
playBank' :: Deck -> Hand -> Hand
playBank' deck bankHand | value bankHand < 16 = playBank' deck' bankHand'
                        | otherwise       = bankHand
  where (deck', bankHand') = draw deck bankHand

playBank :: Deck -> Hand
playBank deck = playBank' deck []
  
-- Task 4B OBS! många arbetstillfällen under veckan försvann pga sjukdom
removeCard :: Int -> Deck -> (Card, Deck)
removeCard n deck = (card, cardBefore ++ cardAfter)
    where
        (cardBefore, card:cardAfter) = splitAt n deck

shuffle :: [Double] -> Deck -> Deck
shuffle _ []        = []
shuffle _ [c]       = [c]
shuffle (d:ds) deck = shuffle ds newDeck
  where
    index = floor (d * fromIntegral (length deck))
    (card, remainingDeck) = removeCard index deck 
    newDeck = card : remainingDeck

-- Koden bör fungera rent logiskt men vi stöter på lite problem och har inte kunnat gå vidare på grund av detta
-- Vi behöver hjälp med vad vår input i GHCI bör vara, shuffle (???) fullDeck
-- ett error dyker upp på shuffle funktionen när vi importerar Test.QuickCheck i koden, hur ska vi röra oss runt detta?

{-
-- Task 5B

belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = undefined

-}
