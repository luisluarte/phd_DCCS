module Simulation.Micro where

import MycelialState
import MycelialStrategy (interpretStrategy, TradingStrategy(..), shouldExecuteBuy, shouldExecuteSell)
import MycelialPhysics (calculateFractalDim, calculateFlowRate, clamVector)
import System.Random (StdGen, randomR)


-- takes a price an agents and returns a maybe with the agent, and 
-- the modification in Capital
executeSell :: Price -> HyphalTip -> Maybe (HyphalTip, Capital, Capital)
executeSell (Price p) agent =
    let
        pos = hypHoldings agent -- this is the bank
        (Quantity q) = posQuantity pos
        (Capital cost) = posCost pos
    in
        if q <= 0 then Nothing
        else
            let
                revenueVal = q * p
                -- this what give us safety, casting Capital newtype over revenueVal
                -- ensure that Capital interacts as double only over the Capital type
                revenue = Capital revenueVal
                profit = Capital (revenueVal - cost)
                (Capital currentBank) = bioBank (hypBiology agent)
                newBank = Capital (currentBank + (revenueVal - cost))
                -- this return agent' with 0 holdings
                newAgent = agent
                    { hypHoldings = mempty
                    , hypRefPrice = Price p
                    , hypStepCount = 0
                    , hypBiology = (hypBiology agent) { bioBank = newBank }
                    }
            in
                Just (newAgent, revenue, profit)

executeTrade :: Price -> HyphalTip -> Capital -> Maybe (HyphalTip, Capital)
executeTrade (Price p) agent (Capital walletBalance) =
    let
    -- get all relevant data from the data structure
        genes = hypGenome agent
        step = hypStepCount agent
        maxOrders = geneMaxOrders genes
    in
    -- agent can only perform maxOrders number of buy orders
        if step >= maxOrders
            then Nothing
            else
                let
	                -- multiplier for DCA buys
                    volMult = if step == 0 then 1.0 else (geneVolMult genes) ^ step
                    -- how complex is the hyphae structure, required to determine flow
                    d = calculateFractalDim (hypPath agent)
                    q_f = calculateFlowRate d
                    baseAmt = if step == 0 then geneBaseOrder genes else geneDCAOrder genes
                    orderCostVal = baseAmt * volMult * q_f
                    -- final check to determine is buy action is possible
                    isAffordable = orderCostVal <= walletBalance
                in
                    if not isAffordable
                        then Nothing
                        else
                            let
                                orderCost = Capital orderCostVal
                                orderQty = Quantity (orderCostVal / p)
                                newPos = (hypHoldings agent) <> Position orderQty orderCost
                                newAgent = agent
                                    {
                                    hypHoldings = newPos,
                                    hypRefPrice = Price p,
                                    hypStepCount = step + 1
                                    }
                            in
                                Just (newAgent, orderCost)

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