-- Fill in the DistanceConversions module first, and import it here
-- create a higher-order function for converting an area between two dimensions
-- this will take the function for converting a distance, and an area to convert
-- using the functions defined in the DistanceConversions module
-- Example areaConv inchesToCentimetres 9 = 58.0644
import DistanceConversions as DC

areaConv :: (Float -> Float) -> Float -> Float
areaConv linearConversion area = (linearConversion (area**(1/2)))**2

-- define a function for converting square inches into square centimetres
sqInToSqCm :: Float -> Float
sqInToSqCm = areaConv  DC.inchesToCentimetres

-- define a function for converting square chains (22 yards) to square metres
sqChainsToSqM :: Float -> Float
sqChainsToSqM = DC.feetToInches . (areaConv (DC.yardsToFeet . (\x -> x*22)))

