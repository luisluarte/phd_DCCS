module Simulation.Lifecycle where

import MycelialState
import qualified Simulation.Types as T
import Simulation.Accessors hiding (Sim)
import Simulation.Micro (executeTrade, executeSell, moveAgent)
import Simulation.Macro (applyDrain, MushroomCache) 
import Simulation.Evolution (mutateGenome)
import MycelialStrategy (interpretStrategy, shouldExecuteBuy, shouldExecuteSell)
import MycelialPhysics (calculatePressure, clampVector)
import qualified Data.Map.Strict as Map
import System.Random (StdGen, mkStdGen, randomR)
import Control.Monad.State (get, modify)
import Data.List (mapAccumL)

-- | 1. Update Hyphal Tip (Agent)
updateHypha :: Bool -> Price -> MushroomCache -> [HyphalTip] -> HyphalTip -> T.Sim (Maybe HyphalTip, [(MushroomId, Capital)], Capital)
updateHypha intel p mushMap allA agent = do
    -- 1. Deduct Maintenance
    let (Capital bank) = bioBank (hypBiology agent)
        maintVal = geneMaintenance (hypGenome agent)
        maint = Capital maintVal
    
    if bank <= maintVal 
        then return (Nothing, [], 0) -- Starvation Death
        else do
            let oldBio = hypBiology agent
            let newAge = bioAge oldBio + 1
            
            -- Update bank AND age
            let agentAfterMaint = agent 
                    { hypBiology = oldBio 
                        { bioBank = Capital (bank - maintVal)
                        , bioAge = newAge 
                        } 
                    }
            
            -- 2. Calculate Pressure & Move (WITH STRESS)
            let (HyphalId hid) = hypId agentAfterMaint
            let (Price priceVal) = p 
            
            let pressure = calculatePressure p agentAfterMaint
            
            -- IMPLEMENTING THE STRESS IDEA:
            -- Metabolic Anxiety: If bank is low, increase turbulence (Panic Searching)
            let geneTurb = geneTurbulence (hypGenome agent)
            let safeThreshold = 20.0
            
            -- Stress Multiplier: 1.0 (Calm) -> ~3.0 (Panic)
            let stressMult = if bank < safeThreshold
                             then 1.0 + (2.0 * ((safeThreshold - bank) / safeThreshold))
                             else 1.0
            
            let effectiveTurbulence = geneTurb * stressMult
            
            -- Generate a seed for movement logic
            let rngSeed = hid + (newAge * 1000)
            let rng = mkStdGen rngSeed
            
            -- FIXED: Pass effectiveTurbulence to moveAgent
            let movedAgent = moveAgent pressure effectiveTurbulence agentAfterMaint rng

            -- 3. Strategy & Trading Logic
            let strategy = interpretStrategy movedAgent p 

            let trySell = shouldExecuteSell strategy
            let tryBuy = shouldExecuteBuy strategy
            
            finalAgentResult <- if trySell
                then case executeSell p movedAgent of
                        Just (soldAgent, _, _) -> return (Just soldAgent) 
                        Nothing -> return (Just movedAgent)
                else if tryBuy
                     then case executeTrade p movedAgent of
                            Just (boughtAgent, _) -> return (Just boughtAgent) 
                            Nothing -> return (Just movedAgent)
                     else return (Just movedAgent)

            -- 4. Apply Vacuum
            case finalAgentResult of
                Nothing -> return (Nothing, [], maint)
                Just traderAgent -> do
                    let (drainedAgent, taxes) = applyDrain traderAgent mushMap p
                    return (Just drainedAgent, taxes, maint)


-- | 2. Update Mushroom Body
updateMushroom :: Bool -> Price -> [(MushroomId, Capital)] -> StdGen -> MushroomBody -> (MushroomBody, [Spore], Capital)
updateMushroom enableMutation currentPrice taxes rng mush =
    let
        -- 1. Absorb Taxes
        myTaxes = sum [amt | (mid, amt) <- taxes, mid == mushId mush]
        
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
                    dispersion = geneDispersion (mushGenome mush)
                    parentLoc = mushLocation mush
                    
                    (sporesCreated, _) = foldl (\(acc, r) _ -> 
                        let 
                            (dx, r1) = randomR (-dispersion, dispersion) r
                            (dy, r2) = randomR (-dispersion, dispersion) r1
                            
                            targetX = max 0.0 (min 1.0 (head parentLoc + dx))
                            targetY = max 0.0 (min 1.0 (parentLoc !! 1 + dy))
                            
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


-- | 3. Germinate Colony (Conserving Mass)
germinateColony :: MushroomId -> HyphalId -> Spore -> Price -> (MushroomBody, [HyphalTip])
germinateColony newMid (HyphalId startHid) spore (Price p) =
    let
        count = geneMaxChildren (sporeGenome spore)
        (Capital availableCap) = sporeCapital spore
        
        -- STRICT CONSERVATION OF MASS
        mushPortion = availableCap * 0.5
        workerPortion = availableCap - mushPortion
        
        perWorkerCap = if count > 0 
                       then workerPortion / fromIntegral count 
                       else 0.0

        newMush = MushroomBody
            { mushId = newMid
            , mushLocation = sporeTarget spore
            , mushMass = Capital mushPortion 
            , mushGenome = sporeGenome spore
            }

        newAgents = 
            [ HyphalTip
                { hypId = HyphalId (startHid + i)
                , hypParentId = newMid
                , hypLocation = sporeTarget spore
                , hypVelocity = [0, 0]
                , hypPath     = [sporeTarget spore]
                , hypHoldings = mempty
                , hypBiology  = BioState 0 (Capital perWorkerCap)
                , hypGenome   = sporeGenome spore
                , hypRefPrice = Price p
                , hypStepCount = 0
                }
            | i <- [0..count-1]
            ]
    in
        (newMush, newAgents)
