module NewHead where

    head :: Monoid a => [a] -> a
    head (x:xs) = x
    head [] = mempty

    example :: [[Int]]
    example = []