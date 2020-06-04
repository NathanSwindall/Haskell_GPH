import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham", (42.0034, -79.00))
                          ,("Austin", (32.0043, 70.034))
                          ,("Johnson City", (40,-90.343))
                          ,("New York", (40.776, -74.545))]

--The globe is not flat, so we need teh Haversine formula
--This uses radians

toRadians :: Double -> Double
toRadians degrees = degrees * pi/180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat,long) = (rlat, rlong)
   where rlat = toRadians lat 
         rlong = toRadians long


haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where (rlat1, rlong1) = latLongToRads coords1
        (rlat2, rlong2) = latLongToRads coords2
        dlat = rlat2 - rlat1
        dlong = rlong2 - rlong1
        a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
        c = 2 * atan2 (sqrt a) (sqrt (1-a))
        earthRadius = 3961.0


printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")


--We need to get two values from the database, find the distance, then print
-- Problem is this
-- We have the haversine function haversine :: LatLong -> LatLong -> Double
--  We need Maybe LatLong -> Maybe LatLong -> Maybe Double

--naivee solution
haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)

arkham = Map.lookup "Arkham" locationDB
austin = Map.lookup "Austin" locationDB
no_city = Nothing

maybeInc = (+) <$> Just 1
useMaybeInt = maybeInc <*> Just 5 -- using a function in context on a value in context

putIntoContext = (++) <$> Just "cats" <*> Just " and dogs"

--Welcome the applicative  Applicative f => f (a -> b) -> f a -> f b  . Functor f => (a -> b) -> f a -> f b


--Let's build the program
main :: IO ()
main = do
    putStrLn "Enter the starting city name:"
    startingInput <- getLine
    let startingCity = Map.lookup startingInput locationDB
    putStrLn "Enter the destination city name:"
    destInput <- getLine
    let destCity = Map.lookup destInput locationDB
    let distance = haversine <$> startingCity <*> destCity
    printDistance distance

