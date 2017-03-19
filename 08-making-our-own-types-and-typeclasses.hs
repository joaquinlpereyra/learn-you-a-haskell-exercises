{-
 - We are going to create some types for a deck of cards
 - The cards need to have an ordering, based on the standard ranking http://en.wikipedia.org/wiki/Standard_52-card_deck#Rank_and_color
 - We are assuming Aces are high.
 - Therefore, the following statements should be true:
 -    (Card Ace Spades) > (Card King Spades)
 -    (Card Two Clubs) < (Card Three Clubs)
 -
 - We are going to provide our own implementation of the Show typeclass for the Card type.
 - When displaying the Card instance in GHCI, or calling show (Card digit suit), the String which should be displayed is "The <Digit> of <Suit>"
 -
 - Uncomment the following declarations to complete the implementation, and provide an implementation for instance Show Card
 -}

data Suit =  Club | Diamond | Heart | Spade 
    deriving (Show, Bounded, Enum)

instance Ord Suit where
    _ <= _ = False

instance Eq Suit where
    _ == _ = True


data Digit = Two | Three | Four | Five | Six | Seven | Nine | Ten | Eleven | Twelve | J | K | Q | Ace
    deriving (Show, Eq, Bounded, Enum, Ord)

data Card = Card { suit :: Suit
                 , digit :: Digit
                 } deriving (Eq, Ord)

instance Show Card where
    show card = "The " ++ (show $ digit card) ++ " of " ++ (show $ suit card)

-- We should be able to provide a function which returns the higher ranked card:
betterCard :: Card -> Card -> Card
betterCard = max

-- Here is a new Typeclass, which represents some kind of playing hand in a game.
-- It returns True for a "winning hand", depending on the rules for the type of class we are playing with
class Hand a where
    play :: [a] -> Bool

-- Implement Hand for Card, where play returns true if the list contains the Ace of Spades
instance Hand Card where
    play = any (\c -> suit c == Spade && digit c == Ace) 

-- Create a new Coin type
data Coin = Tails | Head deriving (Show, Eq, Ord, Bounded, Enum)

-- Implement Hand for Coin, where play returns true if there are ten heads in a row in the list
-- instance Hand Coin where

instance Hand Coin where
    play = all (\c -> c == Head) . take 10 

-- Have a play with implementing Hand for some other types, for instance Int and Bool
