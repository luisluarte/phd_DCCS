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

-- | Moves the agent based on local pressure and turbulence
-- FIXED: Now accepts 'turbulence' (scalar) to determine movement magnitude
moveAgent :: Double -> Double -> HyphalTip -> StdGen -> HyphalTip
moveAgent pressure turbulence agent rng =
    let
        loc = hypLocation agent
        
        -- Base Scale comes from Turbulence (Genome + Stress)
        -- We multiply by a small physics delta (e.g., 0.01) to keep it continuous
        scale = turbulence * 0.01 
        
        (dx, rng1) = randomR (-scale, scale) rng
        (dy, _)    = randomR (-scale, scale) rng1
        
        -- Apply Brownian Motion + Pressure (Gradient Descent/Ascent could go here)
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
        
        baseOrder = geneBaseOrder genome
        maxOrders = geneMaxOrders genome
        currentOrders = hypStepCount agent
        
    in
        if bank > baseOrder && currentOrders < maxOrders
        then 
            let
                pos = hypHoldings agent
                (Quantity currentQ) = posQuantity pos
                (Capital currentCost) = posCost pos
                
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
