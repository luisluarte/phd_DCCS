module Simulation.Micro 
    ( executeTrade
    , executeSell
    , moveAgent
    ) where

import MycelialState
import MycelialStrategy
import MycelialPhysics (calculateFlowRate, clampVector)
import qualified Simulation.Types as T
import Simulation.Accessors hiding (Sim)
import System.Random (StdGen, randomR)

-- ========================================================
-- MOVEMENT LOGIC
-- ========================================================

moveAgent :: Double -> HyphalTip -> StdGen -> HyphalTip
moveAgent pressure agent rng =
    let
        loc = hypLocation agent
        scale = 0.01 
        
        (dx, rng1) = randomR (-scale, scale) rng
        (dy, _)    = randomR (-scale, scale) rng1
        
        newLocRaw = zipWith (+) loc [dx, dy]
        newLoc = clampVector newLocRaw 
    in
        agent 
          { hypLocation = newLoc
          , hypPath = newLoc : hypPath agent 
          }

-- ========================================================
-- TRADING LOGIC
-- ========================================================

-- | Executes a BUY order (Entry)
executeTrade :: Price -> HyphalTip -> Maybe (HyphalTip, Capital)
executeTrade (Price p) agent =
    let 
        genome = hypGenome agent
        (Capital bank) = bioBank (hypBiology agent)
        
        -- PARAMETER CONNECTED: Use geneBaseOrder instead of hardcoded 1.0
        baseOrder = geneBaseOrder genome
        maxOrders = geneMaxOrders genome
        currentOrders = hypStepCount agent
        
    in
        -- CHECK: Sufficient Capital AND Max Orders Check
        if bank > baseOrder && currentOrders < maxOrders
        then 
            let
                pos = hypHoldings agent
                (Quantity currentQ) = posQuantity pos
                (Capital currentCost) = posCost pos
                
                -- Execute Buy
                quantityBought = baseOrder / p
                newPos = Position (Quantity (currentQ + quantityBought)) (Capital (currentCost + baseOrder))
                
                newBank = Capital (bank - baseOrder)
                newStepCount = currentOrders + 1
                
                newAgent = agent
                    { hypHoldings = newPos
                    , hypBiology = (hypBiology agent) { bioBank = newBank }
                    , hypStepCount = newStepCount
                    , hypRefPrice = if currentOrders == 0 then Price p else hypRefPrice agent
                    }
            in
                Just (newAgent, Capital baseOrder)
        else
            Nothing

-- | Executes a SELL order (Exit)
executeSell :: Price -> HyphalTip -> Maybe (HyphalTip, Capital, Capital)
executeSell (Price p) agent =
    let
        pos = hypHoldings agent 
        (Quantity q) = posQuantity pos
        (Capital cost) = posCost pos
    in
        if q <= 1e-9 then Nothing
        else
            let
                revenueVal = q * p
                revenue = Capital revenueVal
                profitVal = revenueVal - cost
                profit = Capital profitVal
                
                (Capital currentBank) = bioBank (hypBiology agent)
                newBank = Capital (currentBank + revenueVal) 
                
                newAgent = agent
                    { hypHoldings = mempty 
                    , hypRefPrice = Price p  
                    , hypStepCount = 0       
                    , hypBiology = (hypBiology agent) { bioBank = newBank }
                    }
            in
                Just (newAgent, revenue, profit)
