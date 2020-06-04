module Main where

isPalindrome :: String -> Bool
isPalindrome text = text == reverse text

main :: IO ()
main = do
    print "Enter a word and I'll let you know if it's a palidrome"
    text <- getLine
    let response = if isPalindrome text
                   then "it is!"
                   else "it's not!"
    print response