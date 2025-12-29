module MycelialStrategy where

import MycelialState

-- ========================================================
-- STRATEGY DEFINITION (PURE PRICE ACTION)
-- ========================================================

data TradingStrategy = TradingStrategy
  { stratDropThreshold :: Double  -- % Price drop required to trigger next buy
  , stratProfitTarget :: Double   -- % Gain required to sell
  } deriving (Show)

-- Index 0: Drop Threshold (Gene determines "How much dip to buy")
-- Index 1: Profit Target (Gene determines "When to take profit")
interpretStrategy :: ParamVector -> TradingStrategy
interpretStrategy vec = 
  let
      -- RAW GENES: Usually come in as 0.0 to 1.0 (or mutated higher).
      -- SCALING FACTOR: 0.05 means the raw 1.0 gene becomes 5%.
      -- This fixes the "Hallucination" where agents waited for 50% drops.
      scale = 0.05 
  in
      TradingStrategy 
        { stratDropThreshold = (vec !! 0) * scale
        , stratProfitTarget  = (vec !! 1) * scale
        }

-- ========================================================
-- EXECUTION LOGIC
-- ========================================================

-- Checks if price is low enough to buy (DCA Logic)
shouldExecuteBuy :: TradingStrategy -> Price -> Price -> Double -> Bool
shouldExecuteBuy strat (Price curr) (Price ref) effectiveDevMult =
    let
        threshold = stratDropThreshold strat
        -- Target = ReferencePrice * (100% - (Threshold * Multiplier))
        -- Example: Ref 100 * (1 - (0.02 * 1.0)) = 98.0
        targetPrice = ref * (1.0 - (threshold * effectiveDevMult))
    in
        curr <= targetPrice

-- Checks if price is high enough to sell (Take Profit Logic)
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
                -- Target = AverageCost * (100% + ProfitTarget)
                -- Example: Cost 100 * (1 + 0.05) = 105.0
                targetPrice = avgCost * (1.0 + stratProfitTarget strat)
            in
                currentPrice >= targetPrice