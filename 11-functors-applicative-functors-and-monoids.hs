import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show)

-- Make the list a Functor
instance Functor List where
    fmap _ Empty = Empty
    fmap f (Value a as) = Value (f a) (fmap f as)

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty bs = bs
combineLists (Value a as) bs = Value a (combineLists as bs)

-- Make our list a Monoid
instance Monoid (List a) where
    mempty = Empty
    mappend = combineLists


-- Make our list an Applicative
instance Applicative List where
    pure a = Value a Empty
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    (Value f fs) <*> (Value a as) = Value (f a) (fs <*> as)

-- Make sure that the List obeys the laws for Applicative and Monoid
-- Monoid rules
-- 1) mempty `mappend` x = x --> True
-- 2) x `mappend` mempty = x --> True
-- 3) (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z) --> True
--
-- Applicative rules
-- too mnay fuck it

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 (Value 20 Empty)
threeValueList = Value 2 (Value 1 (Value 9 Empty))
primesList = Value 2 (Value 3 (Value 5 (Value 7 Empty)))

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2) <$> twoValueList

-- Use <$> and <*> on the lists with a binary function
plusTwo' = (+) <$> twoValueList <*> threeValueList

-- Create some lists of binary functions
binaries = Value (+) $ Value (-) $ Value (*) Empty

-- Use <*> on the binary functions list and the number lists
usingIt = binaries <*> primesList <*> twoValueList
