module Lib
  ( report
  ) where

import           Data.List
import           Numeric.Natural

report =
  intercalate
    "\n"
    [(show turns), (show $ cashsum turns), (show $ turns2cashflows turns)]

--turns = [Noop, Contract 2, Days 10, Contract 1]
turns = [Days 6, Contract 2]

cashsum turns = sum [x | Cash x <- turns]

abc (Accum days (Cashflow orig_cashflow_values)) (Contract contract_days) =
  Accum days zz
  where
    zz = Cashflow $ (map ff yy)
    ff (x, y) = x + y
    yy :: [(Integer, Integer)]
    yy =
      zip
        ((take (fromEnum days) $ repeat 0) ++
        --((take (fromIntegral days) $ repeat 0) ++
         (map (\x -> (toInteger x)) [1 .. contract_days]))
        (orig_cashflow_values ++ (repeat 0))
abc (Accum days cashflow) (Days current_days)  =
  Accum (current_days + days) cashflow
abc (Accum days cashflow) turn = Accum days cashflow

turns2cashflows turns = foldl abc (Accum 0 (Cashflow [])) turns

data Turn
  = Cash Integer
  | Contract Integer
  | Days Natural
  | Noop
  deriving (Show, Eq)

data Accum =
  Accum Natural
        Cashflow
  deriving (Show)

data Cashflow =
  Cashflow [Integer]
  deriving (Show)
