{-# LANGUAGE OverloadedStrings #-}

{-
    This script will demonstrate how to use Data.Text in Haskell. Haskell Strings are very inefficient
    and any project that involves heavy string manipulation should use Data.Text. 

    Questions: 
    What are semigroups
    What are monoids
-}

import qualified Data.Text as T
import Data.Semigroup
import qualified Data.Text.IO as TIO

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

--problem code
myWord :: T.Text --This will throw an error without using the text language extension pragma
myWord = "dog"


--combining text
combinedTextMonoid :: T.Text -- using a monoid
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text --using semigroup
combinedTextSemigroup = "some" <> " " <> "text"



--Sanskrit language
dharma :: T.Text
dharma = "धर्म"

dharma2 :: T.Text
dharma2 = "Nathan"

bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो भयावह"

bgText2 :: T.Text
bgText2 = "Nathan was here right now. Nathan the big dog"

bgText3 :: T.Text 
bgText3 = "네이버 메인에서 다양한 정보와 유용한 컨텐츠를 만나 보세요."

bgText4 :: T.Text 
bgText4 = "쟈쟈쟈쟈쟈저더지ㅐ야니디냐어리댤 ㄹ이ㅏ덩랄아렁 ㄹ아ㅓㄹ 얼아러"

kor :: T.Text 
kor = "요"

kor2 :: T.Text 
kor2 = "쟈"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
   where pieces = T.splitOn query fullText
         highlighted = mconcat ["{", query,"}"] 

main2 = do
    TIO.putStrLn (highlight dharma2 bgText2)


tec = highlight kor2 bgText4

main :: IO ()
main = putStrLn "Name: " *> getLine >>= putStrLn . ("Hello, " <>)