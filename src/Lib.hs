module Lib
  ( report
  ) where

import           Data.List
import           Numeric.Natural

report =
  intercalate
    "\n"
    [(show turns), (show $ cashsum turns), (show $ turns2cashflows turns)]

turns = [AdvanceDays 0, MonthlyIncome 2]

cashsum turns = sum [x | Cash x <- turns]

days_in_a_month = 20

abc (Accum days (Cashflow orig_cashflow_values)) (MonthlyIncome how_many_months) =
  Accum days zz
  where
    zz = Cashflow $ (map ff yy)
    ff (x, y) = x + y
    cc :: [[Integer]]
    cc = replicate (fromEnum how_many_months) dd
    dd :: [Integer]
    dd = map (\(a1, a2) -> a2) (zip [1 .. days_in_a_month] $ [7] ++ rr)
    rr = repeat 0
    xxxx = take (fromEnum days) $ repeat 0
    yy :: [(Integer, Integer)]
    yy = zip (xxxx ++ concat cc) (orig_cashflow_values ++ (repeat 0))
abc (Accum days cashflow) (AdvanceDays how_many_days) =
  Accum (how_many_days + days) cashflow
abc (Accum days cashflow) turn = Accum days cashflow

turns2cashflows turns = foldl abc (Accum 0 (Cashflow [])) turns

data Turn
  = Cash Integer
  | MonthlyIncome Natural
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
