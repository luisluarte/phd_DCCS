module MycelialStrategy where

import MycelialState

data TradingStrategy = Buy | Sell | Hold deriving (Show, Eq)

interpretStrategy :: HyphalTip -> Price -> TradingStrategy
interpretStrategy agent (Price currentP) =
    let (Price refP) = hypRefPrice agent
        (Quantity q) = posQuantity (hypHoldings agent)
        greed = geneGreed (hypGenome agent)
    in if q > 0 && currentP > refP * (1.0 + greed) 
       then Sell 
       else if currentP < refP * (1.0 - geneTurbulence (hypGenome agent)) 
            then Buy 
            else Hold

shouldExecuteBuy, shouldExecuteSell :: TradingStrategy -> Bool
shouldExecuteBuy Buy = True
shouldExecuteBuy _   = False
shouldExecuteSell Sell = True
shouldExecuteSell _    = False
