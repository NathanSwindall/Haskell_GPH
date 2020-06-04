--legacy GamerId was a String
--no GamerId's use an int

import qualified Data.Map as Map 

type UserName = String
type GamerId = Int
type PlayerCredits = Int 

userNameDB :: Map.Map GamerId UserName 
userNameDB = Map.fromList [(1,"dragonSlayer11")
                          ,(2, "LilPrincessSS")
                          ,(3, "SniperWolf1991")
                          ,(4, "xCTHLHUx")
                          ,(5, "NighTTWarrior12")
                          ,(6, "YogSothhot")]

creditDB :: Map.Map UserName PlayerCredits 
creditDB = Map.fromList [("dragonSlayer11",2000)
                        ,("LilPrincessSS", 2032)
                        ,("SniperWolf1991", 33434)
                        ,("xCTHLHUx", 343)
                        ,("NighTTWarrior12",3434)
                        ,("YogSothhot",32)]



--helper functions
lookupUserName :: GamerId -> Maybe UserName 
lookupUserName gId = Map.lookup gId userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditDB

--We want a function that does the following
--Applicative f => f a -> (a -> f b) -> f b

--We could write a wrapper for this function (bad way)
altLookupCredits :: Maybe UserName -> Maybe PlayerCredits 
altLookupCredits Nothing = Nothing 
altLookupCredits (Just username) = lookupCredits username

altcreditsFromId :: GamerId -> Maybe PlayerCredits
altcreditsFromId id = altLookupCredits (lookupUserName id)