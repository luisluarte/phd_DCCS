module MycelialStrategy where

import MycelialState

-- ========================================================
-- STRATEGY DEFINITION (PURE PRICE ACTION)
-- ========================================================

data TradingStrategy = TradingStrategy
  { stratDropThreshold :: Double  -- % Price drop required to trigger next buy
  , stratProfitTarget :: Double   -- % Gain required to sell
  } deriving (Show)

-- Index 0: Drop Threshold
-- Index 1: Profit Target
interpretStrategy :: ParamVector -> TradingStrategy
interpretStrategy vec = 
    let 
        getParam i def = if length vec > i then vec !! i else def
        p0 = abs (getParam 0 0.05)
        p1 = abs (getParam 1 0.10)
    in
        TradingStrategy 
        { stratDropThreshold = max 0.001 p0 
        , stratProfitTarget = max 0.01 p1 
        }

-- ========================================================
-- EXECUTION LOGIC
-- ========================================================

shouldExecuteBuy :: TradingStrategy -> Price -> Price -> Double -> Bool
shouldExecuteBuy strat (Price curr) (Price ref) effectiveDevMult =
    let
        threshold = stratDropThreshold strat
        targetPrice = ref * (1.0 - (threshold * effectiveDevMult))
    in
        curr <= targetPrice

shouldExecuteSell :: TradingStrategy -> Price -> Position -> Bool
shouldExecuteSell strat (Price currentPrice) pos =
    let
        (Quantity q) = posQuantity pos
        (Capital c) = posCost pos
    in
        if q <= 0 then False
        else
            let
                avgCost = c / q
                targetPrice = avgCost * (1.0 + stratProfitTarget strat)
            in
                currentPrice >= targetPrice