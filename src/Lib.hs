module Lib
  ( report
  ) where

import           Data.List
import           Numeric.Natural

report = intercalate "\n" [(show turns), (show $ turns2cashflows turns)]

turns =
  [ AddCashflowShape (PeriodicCashflow days_in_a_month 15 3)
  --, AdvanceDays 1
  --, AddCashflow (peridoc_cashflow days_in_a_month 15 3)
  ]

type HowManyTimes = Natural

type HowManyDays = Natural

type Cash = Integer

days_in_a_month = 20 :: HowManyDays

peridoc_cashflow :: HowManyDays -> Cash -> HowManyDays -> Cashflow
peridoc_cashflow how_often how_much how_many_months = monthly_cash_series
  where
    monthly_cash_series :: Cashflow
    monthly_cash_series =
      concat $ replicate (fromEnum how_many_months) monthly_cash
    monthly_cash :: Cashflow
    monthly_cash =
      map
        (\(a1, a2) -> a2)
        (zip [1 .. days_in_a_month] $ [how_much] ++ (repeat 0))

accumulate_cashflow (AccumulatedCashflow days cashflow) (AdvanceDays how_many_days) =
  AccumulatedCashflow (how_many_days + days) cashflow
accumulate_cashflow (AccumulatedCashflow days orig_cashflow) (AddCashflow additional_cashflow) =
  AccumulatedCashflow days cashflow
  where
    cashflow = xcashflow days additional_cashflow orig_cashflow
accumulate_cashflow (AccumulatedCashflow days orig_cashflow) (AddCashflowShape additional_cashflowshape) =
  AccumulatedCashflow days cashflow
  where
    cashflow = xcashflow days additional_cashflow orig_cashflow
    additional_cashflow = (cashflowshape2cashflow additional_cashflowshape)

cashflowshape2cashflow (PeriodicCashflow how_often how_much how_many_times) =
  peridoc_cashflow how_often how_much how_many_times

xcashflow initial_dead_days additional_cashflow orig_cashflow = cashflow
  where
    initial_dead_period = take (fromEnum initial_dead_days) $ repeat 0
    cash_pairs :: [(Cash, Cash)]
    cash_pairs =
      zip
        (initial_dead_period ++ additional_cashflow)
        (orig_cashflow ++ (repeat 0))
    cashflow :: Cashflow
    cashflow = map (\(x, y) -> x + y) cash_pairs

turns2cashflows turns =
  case vvv of
    AccumulatedCashflow x y -> y
  where
    vvv :: AccumulatedCashflow
    vvv = foldl accumulate_cashflow (AccumulatedCashflow 0 []) turns

data CashflowShape =
  PeriodicCashflow HowManyDays
                   Cash
                   HowManyTimes
  deriving (Show, Eq)

data Turn
  = AddCash Cash
  | AddCashflow Cashflow
  | AddCashflowShape CashflowShape
  | AdvanceDays HowManyDays
  | Noop
  deriving (Show, Eq)

data AccumulatedCashflow =
  AccumulatedCashflow HowManyDays
                      Cashflow
  deriving (Show)

type Cashflow = [Cash]
