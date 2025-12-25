module Simulation.Lifecycle where

import MycelialState
import Simulation.Accessors
import Simulation.Micro (executeTrade, executeSell, moveAgent)
import Simulation.Macro (applyDrain, TaxMap, MushroomCache) -- Import Cache type
import Simulation.Evolution (mutateGenome)
import MycelialPhysics (calculatePressure, clampVector)
import MycelialStrategy (interpretStrategy, TradingStrategy(..), shouldExecuteBuy, shouldExecuteSell)
import System.Random (StdGen, mkStdGen, randomR)
import Control.Monad.State (get, modify)
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map -- Ensure Map is available

dieThresh :: Double
dieThresh = -50.0

-- UPDATED: Takes MushroomCache instead of [MushroomBody]
updateHypha :: Price -> MushroomCache -> [HyphalTip] -> HyphalTip -> Sim (Maybe HyphalTip, TaxMap)
updateHypha currentPrice mushCache allAgents agent = do
    
    -- Pass cache to applyDrain
    let (agentAfterTax, taxes) = applyDrain agent mushCache currentPrice
    
    let psi = calculatePressure currentPrice agentAfterTax
    let (Capital bank) = bioBank (hypBiology agentAfterTax) 

    if psi < dieThresh || bank < 0 
        then do
            modifyWallet (\c -> c + Capital bank)
            return (Nothing, taxes)
        else do
            let strategy = interpretStrategy (hypLocation agent)
            let genes = hypGenome agent
            let devMult = if (hypStepCount agent) == 0 then 1.0 else (geneDevMult genes) ^ (hypStepCount agent)
            
            let shouldSell = shouldExecuteSell strategy currentPrice (hypHoldings agentAfterTax)
            
            agentAfterLogic <- if shouldSell
                then do
                    case executeSell currentPrice agentAfterTax of
                        Just (soldAgent, revenue, _) -> do
                            modifyWallet (\c -> c + revenue)
                            
                            -- LOGGING SELL
                            t <- getTime
                            let (Quantity q) = posQuantity (hypHoldings agentAfterTax)
                            let logEntry = TransactionLog 
                                    { tlHyphaId = hypId agent
                                    , tlType = ActionSell
                                    , tlCost = revenue 
                                    , tlPrice = currentPrice
                                    , tlQuantity = Quantity q
                                    , tlTime = t
                                    }
                            modify $ \s -> s { sysLogs = logEntry : sysLogs s }
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

                                    -- LOGGING BUY
                                    t <- getTime
                                    let (Capital costVal) = cost
                                    let (Price pVal) = currentPrice
                                    let qty = Quantity (costVal / pVal)
                                    let logEntry = TransactionLog 
                                            { tlHyphaId = hypId agent
                                            , tlType = ActionBuy
                                            , tlCost = -cost 
                                            , tlPrice = currentPrice
                                            , tlQuantity = qty
                                            , tlTime = t
                                            }
                                    modify $ \s -> s { sysLogs = logEntry : sysLogs s }
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

-- (Keep updateMushroom and germinateColony as they were, they are fine)
updateMushroom :: Price -> TaxMap -> StdGen -> MushroomBody -> (MushroomBody, [Spore], Capital)
updateMushroom (Price _) income rng mBody =
    let
        myIncome = sum [amt | (mid, amt) <- income, mid == mushId mBody]
        massAfterIncome = (mushMass mBody) + myIncome
        genes = mushGenome mBody
        maintenanceCost = Capital (geneMaintenance genes)
        massAfterCost = massAfterIncome - maintenanceCost
        maturity = geneMaturity genes
        (Capital mVal) = massAfterCost
    in
        if mVal > maturity
            then
                let
                    gamma = geneReproductiveInvest genes
                    batchSize = geneSporeBatchSize genes
                    totalSacrifice = mVal * gamma
                    perSporeEndowment = totalSacrifice / fromIntegral batchSize
                    massAfterSporulation = massAfterCost - Capital totalSacrifice
                    (MushroomId midInt) = mushId mBody
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
        loc = sporeTarget spore
        (Capital totalCap) = sporeCapital spore
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