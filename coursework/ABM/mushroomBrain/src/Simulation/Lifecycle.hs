module Simulation.Lifecycle where

import MycelialState
import qualified Simulation.Types as T
import Simulation.Accessors hiding (Sim)
import Simulation.Micro (executeTrade, executeSell, moveAgent)
import Simulation.Evolution (mutateGenome)
import MycelialStrategy (interpretStrategy, shouldExecuteBuy, shouldExecuteSell)
import MycelialPhysics (calculatePressure, clampVector)
import qualified Data.Map.Strict as Map
import System.Random (StdGen, mkStdGen, randomR)
import Control.Monad.State (get, modify)
import Data.List (mapAccumL)

-- | 1. Update Hyphal Tip (Agent)
updateHypha :: Bool -> Price -> Map.Map MushroomId any -> [HyphalTip] -> HyphalTip -> T.Sim (Maybe HyphalTip, [(MushroomId, Capital)], Capital)
updateHypha intel p mushMap allA agent = do
    -- 1. Deduct Maintenance
    let (Capital bank) = bioBank (hypBiology agent)
        maintVal = geneMaintenance (hypGenome agent)
        maint = Capital maintVal
    
    if bank <= maintVal 
        then return (Nothing, [], 0) -- Starvation Death
        else do
            let agentAfterMaint = agent { hypBiology = (hypBiology agent) { bioBank = Capital (bank - maintVal) } }
            
            -- 2. Calculate Pressure & Move
            let (HyphalId hid) = hypId agentAfterMaint
            let (Price priceVal) = p -- FIXED: Match Price, not Capital
            
            let pressure = calculatePressure p agentAfterMaint
            
            -- Generate a seed for movement logic
            let rngSeed = hid + (bioAge (hypBiology agentAfterMaint) * 1000)
            let rng = mkStdGen rngSeed
            
            let movedAgent = moveAgent pressure agentAfterMaint rng

            -- 3. Strategy & Trading Logic
            -- FIXED: interpretStrategy takes (HyphalTip -> Price -> TradingStrategy)
            let strategy = interpretStrategy movedAgent p 

            -- FIXED: shouldExecute* functions only check the strategy enum
            let trySell = shouldExecuteSell strategy
            let tryBuy = shouldExecuteBuy strategy
            
            finalAgentResult <- if trySell
                then case executeSell p movedAgent of
                        Just (soldAgent, _, _) -> return (Just soldAgent) -- Successful Sell
                        Nothing -> return (Just movedAgent)
                else if tryBuy
                     then case executeTrade p movedAgent of
                            Just (boughtAgent, _) -> return (Just boughtAgent) -- Successful Buy
                            Nothing -> return (Just movedAgent)
                     else return (Just movedAgent)

            case finalAgentResult of
                Just ag -> return (Just ag, [], maint)
                Nothing -> return (Nothing, [], maint)


-- | 2. Update Mushroom Body
updateMushroom :: Bool -> Price -> [(MushroomId, Capital)] -> StdGen -> MushroomBody -> (MushroomBody, [Spore], Capital)
updateMushroom enableMutation currentPrice taxes rng mush =
    let
        -- 1. Absorb Taxes
        -- Calculate total taxes for this mushroom
        myTaxes = sum [amt | (mid, amt) <- taxes, mid == mushId mush]
        
        -- FIXED: Use Num instance for Capital addition (mushMass is Capital, myTaxes is Capital)
        newMass = (mushMass mush) + myTaxes
        
        -- 2. Check Maturity for Sporulation
        maturityThreshold = geneMaturity (mushGenome mush)
        reproInvest = geneReproductiveInvest (mushGenome mush)
        
        (finalMush, spores, spentMass) = if newMass > Capital maturityThreshold
            then
                let
                    investment = newMass * (Capital reproInvest)
                    sporeCount = geneSporeBatchSize (mushGenome mush)
                    costPerSpore = investment / (fromIntegral sporeCount)
                    
                    (sporesCreated, _) = foldl (\(acc, r) _ -> 
                        let 
                            (targetX, r1) = randomR (0.0, 1.0) r
                            (targetY, r2) = randomR (0.0, 1.0) r1
                            childGenome = if enableMutation 
                                          then mutateGenome (mushGenome mush) r2 
                                          else mushGenome mush
                            newSpore = Spore 
                                { sporeTarget = [targetX, targetY]
                                , sporeGenome = childGenome
                                , sporeCapital = costPerSpore 
                                }
                        in (newSpore : acc, r2)
                        ) ([], rng) [1..sporeCount]

                    mushAfterRepro = mush { mushMass = newMass - investment }
                in
                    (mushAfterRepro, sporesCreated, investment)
            else
                (mush { mushMass = newMass }, [], 0)
    in
        (finalMush, spores, spentMass)


-- | 3. Germinate Colony
germinateColony :: MushroomId -> HyphalId -> Spore -> Price -> (MushroomBody, [HyphalTip])
germinateColony newMid (HyphalId startHid) spore (Price p) =
    let
        newMush = MushroomBody
            { mushId = newMid
            , mushLocation = sporeTarget spore
            , mushMass = sporeCapital spore
            , mushGenome = sporeGenome spore
            }

        count = 5 
        newAgents = 
            [ HyphalTip
                { hypId = HyphalId (startHid + i)
                , hypParentId = newMid
                , hypLocation = sporeTarget spore
                , hypVelocity = [0, 0]
                , hypPath     = [sporeTarget spore]
                , hypHoldings = mempty
                , hypBiology  = BioState 0 (Capital 10.0)
                , hypGenome   = sporeGenome spore
                , hypRefPrice = Price p
                , hypStepCount = 0
                }
            | i <- [0..count-1]
            ]
    in
        (newMush, newAgents)
