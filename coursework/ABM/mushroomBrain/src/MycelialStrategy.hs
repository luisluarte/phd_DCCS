module MycelialStrategy where

import MycelialState

data TradingAction = Buy | Sell | Hold deriving (Show, Eq)

-- | Maps the agent's internal state and genome to a market action
interpretStrategy :: HyphalTip -> Price -> TradingAction
interpretStrategy agent (Price currentPrice) =
    let genes = hypGenome agent
        pos   = hypHoldings agent
        avg   = hypAvgEntry agent
        (Price avgP) = avg
        (Quantity q) = posQuantity pos
        
        -- Categorical Hurdle: Value Preservation
        -- Only sell if current price exceeds weighted cost + greed margin
        profitTarget = avgP * (1.0 + geneGreed genes)
        
        -- DCA Logic: Buy if price is below current average (lowering cost basis)
        buyHurdle = avgP * (1.0 - geneTurbulence genes)
    in case () of
        _ | q > 0 && currentPrice >= profitTarget -> Sell
        _ | currentPrice <= buyHurdle || avgP == 0 -> Buy
        _ -> Hold

-- | Utility to calculate the 'Skin in the Game' metric
calculateSkinInGame :: HyphalTip -> Double
calculateSkinInGame agent =
    let pos = hypHoldings agent
        avg = hypAvgEntry agent
        (Capital c) = calculatePosCost pos avg
        (Capital b) = bioBank (hypBiology agent)
    in if (c + b) > 0 then c / (c + b) else 0.0
