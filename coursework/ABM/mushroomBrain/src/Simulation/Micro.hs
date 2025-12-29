module Simulation.Micro where

import MycelialState
import MycelialStrategy (interpretStrategy, TradingStrategy(..), shouldExecuteBuy, shouldExecuteSell)
import MycelialPhysics (calculateFractalDim, calculateFlowRate, clampVector)
import System.Random (StdGen, randomR)

-- ========================================================
-- EXECUTE SELL (Corrected & Clean)
-- ========================================================
executeSell :: Price -> HyphalTip -> Maybe (HyphalTip, Capital, Capital)
executeSell (Price p) agent =
    let
        pos = hypHoldings agent 
        (Quantity q) = posQuantity pos
        (Capital cost) = posCost pos
    in
        if q <= 0 then Nothing
        else
            let
                revenueVal = q * p
                revenue = Capital revenueVal
                profit = Capital (revenueVal - cost)
                
                (Capital currentBank) = bioBank (hypBiology agent)
                
                -- CORRECT MATH: Return Principal + Profit
                newBank = Capital (currentBank + revenueVal) 
                
                newAgent = agent
                    { hypHoldings = mempty
                    , hypRefPrice = Price p
                    , hypStepCount = 0
                    , hypBiology = (hypBiology agent) { bioBank = newBank }
                    }
            in
                Just (newAgent, revenue, profit)

-- ========================================================
-- EXECUTE TRADE (With Safety Buffer)
-- ========================================================
executeTrade :: Price -> HyphalTip -> Maybe (HyphalTip, Capital)
executeTrade (Price p) agent =
    let
        genes = hypGenome agent
        step = hypStepCount agent
        maxOrders = geneMaxOrders genes
    in
        if step >= maxOrders
            then Nothing
            else
                let
                    volMult = if step == 0 then 1.0 else (geneVolMult genes) ^ step
                    d = calculateFractalDim (hypPath agent)
                    q_f = calculateFlowRate d
                    baseAmt = if step == 0 then geneBaseOrder genes else geneDCAOrder genes
                    orderCostVal = baseAmt * volMult * q_f
                    
                    (Capital myBank) = bioBank (hypBiology agent)

                    -- Safety Buffer: 20 ticks of maintenance
                    safetyMargin = (geneMaintenance genes) * 20.0
                    
                    isAffordable = (orderCostVal + safetyMargin) <= myBank
                in
                    if not isAffordable
                        then Nothing
                        else
                            let
                                orderCost = Capital orderCostVal
                                orderQty = Quantity (orderCostVal / p)
                                
                                newBank = Capital (myBank - orderCostVal)
                                
                                newPos = (hypHoldings agent) <> Position orderQty orderCost
                                newAgent = agent
                                    { hypHoldings = newPos
                                    , hypRefPrice = Price p
                                    , hypStepCount = step + 1
                                    , hypBiology = (hypBiology agent) { bioBank = newBank } 
                                    }
                            in
                                Just (newAgent, orderCost)

-- ========================================================
-- MOVEMENT
-- ========================================================
moveAgent :: Double -> HyphalTip -> StdGen -> HyphalTip
moveAgent pressure agent rng =
    let
        genes = hypGenome agent
        psi_crit = geneTurbulence genes
        k = 0.5
        sigmoid = 1.0 / (1.0 + exp (-(k * (pressure - psi_crit))))
        currentLoc = hypLocation agent
        currentVel = hypVelocity agent
        (r1, rng1) = randomR (-1.0, 1.0) rng
        (r2, _) = randomR (-1.0, 1.0) rng1
        randomVec = [r1, r2]
        safeVel = if all (==0) currentVel then randomVec else currentVel
        newVel = zipWith (\v r -> (1.0 - sigmoid) * v + sigmoid * r) safeVel randomVec
        eta = geneGrowthRate genes
        
        rawLoc = zipWith (\x v -> x + eta * v) currentLoc newVel
        newLoc = clampVector rawLoc
    in
        agent { hypLocation = newLoc, hypVelocity = newVel, hypPath = newLoc : hypPath agent }