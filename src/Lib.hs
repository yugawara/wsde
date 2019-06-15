module Lib
    ( someFunc

    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Turn = Cash Integer deriving Show