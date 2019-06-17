module Lib
  ( report
  ) where

import           Data.List
import           Numeric.Natural

ttt = foldr (+) 0 [1 ..]

report =
  intercalate
    "\n"
    [ (show ttt)
    , (show turns)
    , (show $ cashsum turns)
    , (show $ turns2cashflows turns)
    ]

turns = [Noop, Contract, Days 0]

cashsum turns = sum [x | Cash x <- turns]

abc Contract (Accum days cashflow) = Accum 0 cashflow
abc turn (Accum days cashflow)     = Accum 0 cashflow

turns2cashflows turns = foldr abc (Accum 0 (Cashflow [])) turns

data Turn
  = Cash Integer
  | Contract
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
