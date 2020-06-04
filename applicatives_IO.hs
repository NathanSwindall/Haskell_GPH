--Command line program: Must enter number then press enter
minOfThree :: (Ord a) => a -> a -> a -> a 
minOfThree val1 val2 val3 = min val1 (min val2 val3) -- pretty cool way of doing this

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do 
    putStrLn "Enter three numbers"
    minInt <- minOfInts
    putStrLn (show minInt ++ " is the smallest number")


data User = User_Con
    { name :: String
    , gamerId :: Int
    , score :: Int
    } deriving Show


serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

user_maybe = User_Con <$> serverUsername <*> serverGamerId <*> serverScore
user_may = User_Con <$> serverUsername <*> serverScore <*> serverGamerId -- notice that this is in the wrong order

doorPrize :: [Int]
doorPrize = [1000, 2000,3000]

boxPrize :: [Int]
boxPrize = [500, 20000]

--notdeterministically
totalPrize :: [Int]
totalPrize = (pure (+)) <*> doorPrize <*> boxPrize

--generate prime Numbers
infinite_list = [2..]
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
   where twoThroughN = [2..n]
         composite = pure (*) <*> twoThroughN <*> twoThroughN
         isNotComposite = not . (`elem` composite)


testNames :: [String]
testNames = ["John Smith"
            , "Robert'; DROP TABLE Students;--"
            , "Christina NULL"
            , "Randall Munroe"]

testIds :: [Int]
testIds = [1337,
           0123
           , 9999999]

testScores :: [Int]
testScores = [0
             ,100000000
             , 9999999]


--These are the same and create a bunch of test data
testData = User_Con <$> testNames <*> testIds <*> testScores
testData2 = pure User_Con <*> testNames <*> testIds <*> testScores
    --pure puts the User_Con in context and then will take the other arguments

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func context = pure func <*> context