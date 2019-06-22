module Lib
  ( report
  ) where

import           Data.List
import           Numeric.Natural

report =
  intercalate
    "\n"
    [(show turns), (show $ cashsum turns), (show $ turns2cashflows turns)]

turns = [AdvanceDays 0, MonthlyCash 15 3]

cashsum turns = sum [x | Cash x <- turns]

days_in_a_month = 20

abc (Accum days (Cashflow orig_cashflow_values)) (MonthlyCash how_much how_many_months) =
  Accum days cashflow
  where
    cashflow :: Cashflow
    cashflow = Cashflow $ (map (\(x, y) -> x + y) cash_pairs)
    monthly_cash_series :: [Integer]
    monthly_cash_series =
      concat $ replicate (fromEnum how_many_months) monthly_cash
    monthly_cash :: [Integer]
    monthly_cash =
      map
        (\(a1, a2) -> a2)
        (zip [1 .. days_in_a_month] $ [how_much] ++ (repeat 0))
    initial_dead_period = take (fromEnum days) $ repeat 0
    cash_pairs :: [(Integer, Integer)]
    cash_pairs =
      zip
        (initial_dead_period ++ monthly_cash_series)
        (orig_cashflow_values ++ (repeat 0))
abc (Accum days cashflow) (AdvanceDays how_many_days) =
  Accum (how_many_days + days) cashflow
abc (Accum days cashflow) turn = Accum days cashflow

turns2cashflows turns = foldl abc (Accum 0 (Cashflow [])) turns

data Turn
  = Cash Integer
  | MonthlyCash Integer
                Natural
  | AdvanceDays Natural
  | Noop
  deriving (Show, Eq)

data Accum =
  Accum Natural
        Cashflow
  deriving (Show)

data Cashflow =
  Cashflow [Integer]
  deriving (Show)
