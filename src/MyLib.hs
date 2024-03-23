module MyLib where

import Html

someFunc :: IO ()
someFunc = putStrLn . render $ myhtml

myhtml =
     html_ "My title"
     ( (<>) (h_ 1 "Heading")
     ( (<>) (p_ "P1")
            (p_ "P2")
     )
     )
