module Simulation.Lifecycle where

import MycelialState
import Simulation.Accessors
import Simulation.Micro (executeTrade, executeSell, moveAgent)
import Simulation.Macro (applyDrain, TaxMap)
import Simulation.Evolution (mutateGenome)
import MycelialPhysics (calculatePressure, clampVector)
import MycelialStrategy (interpretStrategy, TradingStrategy(..), shouldExecuteBuy, shouldExecuteSell)
import System.Random (StdGen, mkStdGen, randomR)
import Control.Monad.State (get)
import Data.List (mapAccumL)

-- constants (TODO: put it in config module)
dieThresh :: Double
dieThresh = -50.0


-- hyphal lifecycle (agent update loop)
updateHypha :: Price -> [MushroomBody] -> [HyphalTip] -> HyphalTip -> Sim (Maybe HyphalTip, TaxMap)
updateHypha currentPrice mushrooms allAgents agent = do
    let (agentAfterTax, taxes) = applyDrain agent mushrooms currentPrice allAgents
    let psi = calculatePressure currentPrice agentAfterTax
    let (Capital bank) = bioBank (hypBiology agentAfterTax) 

    if psi < dieThresh || bank < 0 -- this is a hyphae that's going to be removed
        then do
            modifyWallet (\c -> c + Capital bank) -- Capital bank is the constructor, so types are the same
            return (Nothing, taxes)
        else do
            -- interpretStrategy starts the TradingStrategy data type
            -- which includes the strategy: drop threshold and take profit
            let strategy = interpretStrategy (hypLocation agent)
            -- get the hyphae genome
            let genes = hypGenome agent
            -- hypStepCount reset every time the agent sells
            let devMult = if (hypStepCount agent) == 0 then 1.0 else (geneDevMult genes) ^ (hypStepCount agent)
            
            let shouldSell = shouldExecuteSell strategy currentPrice (hypHoldings agentAfterTax)
            
            agentAfterLogic <- if shouldSell
                then do
                    -- checks is making this sell is actually possible
                    case executeSell currentPrice agentAfterTax of
                        Just (soldAgent, revenue, _) -> do
                            modifyWallet (\c -> c + revenue)
                            return soldAgent
                        Nothing -> return agentAfterTax
                else do
                    let shouldBuy = shouldExecuteBuy strategy currentPrice (hypRefPrice agentAfterTax) devMult
                    if shouldBuy
                        then do
                            (GlobalWallet balance) <- getWallet
                            case executeTrade currentPrice agentAfterTax balance of
                                Just (boughtAgent, cost) -> do
                                    modifyWallet (\c -> c - cost)
                                    return boughtAgent
                                Nothing -> return agentAfterTax 
                        else return agentAfterTax

            t <- getTime
            let (Time tick) = t
            let (HyphalId hid) = hypId agent
            let rng = mkStdGen (hid + tick * 1000)
            let agentAfterMove = moveAgent psi agentAfterLogic rng
            let bio = hypBiology agentAfterMove
            let finalAgent = agentAfterMove { hypBiology = bio { bioAge = bioAge bio + 1 } }

            return (Just finalAgent, taxes)


updateMushroom :: Price -> TaxMap -> StdGen -> MushroomBody -> (MushroomBody, [Spore], Capital)
updateMushroom (Price _) income rng mBody =
    let
    -- result of the sinking operation is to increase mushroom mass
        myIncome = sum [amt | (mid, amt) <- income, mid == mushId mBody]
        massAfterIncome = (mushMass mBody) + myIncome
        genes = mushGenome mBody
        
        -- there's a cost of being alive :c
        maintenanceCost = Capital (geneMaintenance genes)
        massAfterCost = massAfterIncome - maintenanceCost
        
        -- maturity controls when to start sporulation
        maturity = geneMaturity genes
        (Capital mVal) = massAfterCost
    in
        if mVal > maturity
            then
                let
                -- how much capital is going to be assigned to the spores
                    gamma = geneReproductiveInvest genes
                    -- how much spores to release
                    batchSize = geneSporeBatchSize genes

                    -- the sacrifice is here
                    totalSacrifice = mVal * gamma
                    perSporeEndowment = totalSacrifice / fromIntegral batchSize
                    massAfterSporulation = massAfterCost - Capital totalSacrifice
                    (MushroomId midInt) = mushId mBody

                    -- here we deal with sporulation
                    generateSpore currentRng i =
                        let
                            seed = i * 13 + round mVal + (midInt * 7918)
                            (mutatedGenes) = mutateGenome genes (mkStdGen seed)
                            
                            (r1, rng1) = randomR (-1.0, 1.0) currentRng
                            (r2, rng2) = randomR (-1.0, 1.0) rng1
                            
                            disp = geneDispersion genes
                            target = zipWith (+) (mushLocation mBody) [r1 * disp, r2 * disp]
                            clampedTarget = clampVector target
                            
                            spore = Spore
                                { sporeTarget = clampedTarget
                                , sporeGenome = mutatedGenes
                                , sporeCapital = Capital perSporeEndowment
                                }
                        in
                            (rng2, spore)

                    (_, newSpores) = mapAccumL generateSpore rng [1..batchSize]

                    finalMushroom = mBody { mushMass = massAfterSporulation }
                in
                    (finalMushroom, newSpores, maintenanceCost)
            else
                (mBody { mushMass = massAfterCost }, [], maintenanceCost)



germinateColony :: MushroomId -> HyphalId -> Spore -> Price -> (MushroomBody, [HyphalTip])
germinateColony mid (HyphalId startAid) spore currentPrice =
    let
        genes = sporeGenome spore
        -- hyphae are spawn at the same spot were the spore landed
        loc = sporeTarget spore
        (Capital totalCap) = sporeCapital spore

        -- the mass is equally split between the mushroom
        -- and at all of its hyphae
        nChildren = max 1 (geneMaxChildren genes)
        divisor = fromIntegral nChildren + 1.0
        shareSize = totalCap / divisor

        newMushroom = MushroomBody
            { mushId = mid
            , mushLocation = loc
            , mushMass = Capital shareSize
            , mushGenome = genes
        }

        createWorker i = HyphalTip
            { hypId = HyphalId (startAid + i)
            , hypParentId = mid
            , hypLocation = loc
            , hypVelocity = [0,0]
            , hypPath = [loc]
            , hypHoldings = mempty
            , hypBiology = BioState { bioAge = 0, bioBank = Capital shareSize }
            , hypGenome = genes
            , hypRefPrice = currentPrice
            , hypStepCount = 0
        }

        newHyphae = map createWorker [0..(nChildren - 1)]

    in
        (newMushroom, newHyphae)