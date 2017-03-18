-- Sum the numbers between two inclusive values recursively, assuming a < b when the function is first called
-- Example: sumInts 0 1 = 1
--          sumInts 1 3 = 6
--          sumInts 1 4 = 5 + 5 = 10
sumInts :: Int -> Int -> Int
sumInts x y 
    | x > y = 0
    | x == y = x
    | x < y = x + y + sumInts (x+1) (y-1)

-- Define a square function
sq :: Int -> Int
sq x = x * x

-- Sum the squares between two numbers. This function should be similar to the sumInts function
sumSquares :: Int -> Int -> Int
sumSquares x y 
    | x > y = 0
    | x == y = sq x
    | x < y = sq x + sq y + sumSquares (x+1) (y-1)

-- Define a higher order sum function which accepts an (Int -> Int) function to apply to all integers between two values.
-- Again this should look similar to the sumInts and sumSquares functions
higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum f x y 
    | x > y = 0
    | x == y = f x 
    | x < y = (f x) + (f y) + (higherOrderSum f (x+1) (y-1))

-- Define the square sum in terms of higherOrderSum
hoSumSquares :: Int -> Int -> Int
hoSumSquares = higherOrderSum sq

-- Define the sum between two values in terms of higherOrderSum
-- Note there is no parameter on the function definition
-- Try to use a lambda if possible
hoSumInts :: Int -> Int -> Int
hoSumInts = higherOrderSum (\x -> x) 

-- Create a new higher order method which generalises over the function provided by sumInts (That is, parameterize (+) :: Int -> Int -> Int) between a and b
-- This will give the ability to perform utilities such as the prodcut of all squares (or any other Int -> Int function) between a and b
-- You will also need to generalise the base case
-- You can also define the function signature yourself, which leaves you free to define the parameters and their order
-- To be clear, your function will need to handle:
--  - A start value, a :: Int
--  - A end value, b :: Int
--  - A function to apply to each value, op :: Int -> Int
--  - A function to apply between each value, f :: Int -> Int -> Int
--  - A value to return in the base case when a > b, z :: Int

higherOrderSequenceApplication :: Int -> Int -> (Int -> Int) -> (Int -> Int -> Int) -> Int -> Int
higherOrderSequenceApplication x y which op z 
    | x > y = z
    | x == y = which x
    | x < y = op (op (which x) (which y)) (higherOrderSequenceApplication (x+1) (y-1) which op  z)

-- Define a factorial method using the higherOrderSequenceAppliction
hoFactorial :: Int -> Int
hoFactorial x = higherOrderSequenceApplication 1 x (\x -> x) (*) 1
