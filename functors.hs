import qualified Data.Map as Map

--One way to look at the Functor class is things that can be mapped overs
successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing


--functor: fmap takes something out of context, does the required function, then puts it back in context
--- Just 6 ->  (+1) ->  Just (6 + 1)
-- instance Functor Maybe where
--  fmap func (Just n) = Just (func n)
--  fmap func Nothing = Nothing
functor_fmap = fmap (+1) successfulRequest
functor_binary = (+1) <$> successfulRequest

--data example
data Nathan = Work {
    hours :: Int
    ,place :: String
} | Home {
    place :: String
    ,hobby :: String
} deriving Show



--Robot example
data RobotPart = RobotPart
     { name :: String
     , description :: String
     , cost :: Double
     , count :: Int
     } deriving Show


leftArm :: RobotPart
leftArm = RobotPart
    { name = "left arm"
    , description = "left arm for face punching"
    , cost = 1000.00
    , count = 3}

rightArm :: RobotPart
rightArm = RobotPart
    { name = "right arm"
    , description = "right arm for kind hand gestures"
    , cost = 1025.00
    , count = 5
    }

robotHead :: RobotPart
robotHead = RobotPart 
    { name  = "robot head"
    , description = "this head looks mad"
    , cost = 5092.25
    , count = 2
    }

type Html = String -- Type alias

renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>", partName, "</h2>" --mconcat is a semigroup function
                          , "<p> <h3>desc</h3>",partDesc
                          , "</p><p><h3>cost</h3>"
                          , partCost
                          , "</p><p><h3>count</h3>"
                          , partCount, "</p>"]
    where partName = name part 
          partDesc = description part 
          partCost = show (cost part) 
          partCount = show (count part)



       
--let's use a key value map 
partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where keys = [1,2,3]
        vals = [leftArm, rightArm, robotHead]
        keyVals = zip keys vals
        
--insertSnippet :: Maybe Html -> IO ()

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal -- notice the the function is being applied to the inside of maybe

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)


allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

--Map is an instance of a Functor
-- Map has kind * -> * -> * but a functor is * -> * 
-- It works on map because it is only taking into account the values
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB


leftArmIO :: IO RobotPart
leftArmIO = return leftArm -- return wraps the value in an IO

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO







