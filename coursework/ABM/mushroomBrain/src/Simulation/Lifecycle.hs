module Simulation.Lifecycle where

import MycelialState
import Simulation.Accessors
import Simulation.Micro (executeTrade, executeSell, moveAgent)
import Simulation.Macro (applyDrain, TaxMap, MushroomCache) 
import Simulation.Evolution (mutateGenome)
import MycelialPhysics (calculatePressure, clampVector)
import MycelialStrategy (interpretStrategy, TradingStrategy(..), shouldExecuteBuy, shouldExecuteSell)
import System.Random (StdGen, mkStdGen, randomR)
import Control.Monad.State (get, modify)
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map 

dieThresh :: Double
dieThresh = -50.0

updateHypha :: Bool -> Price -> MushroomCache -> [HyphalTip] -> HyphalTip -> Sim (Maybe HyphalTip, TaxMap, Capital)
updateHypha enableIntelligence currentPrice mushCache allAgents agent = do
    
    let (agentAfterTax, taxes) = applyDrain agent mushCache currentPrice
    
    -- METABOLIC COST
    let genes = hypGenome agent
    let maintCostVal = geneMaintenance genes
    let maintCost = Capital maintCostVal
    
    let (Capital currentBank) = bioBank (hypBiology agentAfterTax)
    let newBankVal = currentBank - maintCostVal
    
    let agentAfterCost = agentAfterTax 
          { hypBiology = (hypBiology agentAfterTax) { bioBank = Capital newBankVal } }

    t <- getTime
    let (Time tick) = t
    let (HyphalId hid) = hypId agent
    
    let signalPsi = calculatePressure currentPrice agentAfterCost 
    let noiseSeed = hid + tick * 7919 
    let noiseRng = mkStdGen noiseSeed
    let (noisePsi, _) = randomR (-100.0, 100.0) noiseRng 
    let psi = if enableIntelligence then signalPsi else noisePsi

    if psi < dieThresh || newBankVal < 0 
        then do
            modifyWallet (\c -> c + max 0 (Capital newBankVal))
            return (Nothing, taxes, maintCost)
        else do
            let strategy = interpretStrategy (hypLocation agent)
            let devMult = if (hypStepCount agent) == 0 then 1.0 else (geneDevMult genes) ^ (hypStepCount agent)
            
            let shouldSell = shouldExecuteSell strategy currentPrice (hypHoldings agentAfterCost)
            
            agentAfterLogic <- if shouldSell
                then do
                    case executeSell currentPrice agentAfterCost of
                        Just (soldAgent, revenue, profit) -> do
                            modifyWallet (\c -> c - revenue)
                            let (Quantity q) = posQuantity (hypHoldings agentAfterCost)
                            let logEntry = TransactionLog 
                                    { tlHyphaId = hypId agent, tlType = ActionSell, tlCost = revenue, tlPrice = currentPrice, tlQuantity = Quantity q, tlTime = t }
                            modify $ \s -> s { sysLogs = logEntry : sysLogs s }
                            return soldAgent
                        Nothing -> return agentAfterCost
                else do
                    -- Forced Entry Logic
                    let shouldBuy = (hypStepCount agent == 0) || shouldExecuteBuy strategy currentPrice (hypRefPrice agentAfterCost) devMult

                    if shouldBuy
                        then do
                            case executeTrade currentPrice agentAfterCost of
                                Just (boughtAgent, cost) -> do
                                    modifyWallet (\c -> c + cost)
                                    let (Capital costVal) = cost
                                    let qty = Quantity (costVal / (let (Price p) = currentPrice in p))
                                    let logEntry = TransactionLog 
                                            { tlHyphaId = hypId agent, tlType = ActionBuy, tlCost = -cost, tlPrice = currentPrice, tlQuantity = qty, tlTime = t }
                                    modify $ \s -> s { sysLogs = logEntry : sysLogs s }
                                    return boughtAgent

                                -- FIX: BREAK THE POVERTY LOOP
                                -- If trade fails (too poor), we MUST increment step count.
                                -- Otherwise, step stays 0, forcing a buy attempt every tick until death.
                                Nothing -> 
                                    return agentAfterCost { hypStepCount = hypStepCount agentAfterCost + 1 }

                        else return agentAfterCost

            let moveRng = mkStdGen (hid + tick * 1000)
            let agentAfterMove = moveAgent psi agentAfterLogic moveRng
            let bio = hypBiology agentAfterMove
            let finalAgent = agentAfterMove { hypBiology = bio { bioAge = bioAge bio + 1 } }

            return (Just finalAgent, taxes, maintCost)

-- (Keep updateMushroom and germinateColony as they are)
updateMushroom :: Bool -> Price -> TaxMap -> StdGen -> MushroomBody -> (MushroomBody, [Spore], Capital)
updateMushroom enableMutation (Price _) income rng mBody =
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
                            (mutatedGenes) = if enableMutation
                                             then mutateGenome genes (mkStdGen seed)
                                             else genes
                            (r1, rng1) = randomR (-1.0, 1.0) currentRng
                            (r2, rng2) = randomR (-1.0, 1.0) rng1
                            disp = geneDispersion genes
                            target = zipWith (+) (mushLocation mBody) [r1 * disp, r2 * disp]
                            clampedTarget = clampVector target
                            spore = Spore
                                { sporeTarget = clampedTarget, sporeGenome = mutatedGenes, sporeCapital = Capital perSporeEndowment }
                        in (rng2, spore)
                    (_, newSpores) = mapAccumL generateSpore rng [1..batchSize]
                    finalMushroom = mBody { mushMass = massAfterSporulation }
                in (finalMushroom, newSpores, maintenanceCost)
            else (mBody { mushMass = massAfterCost }, [], maintenanceCost)

germinateColony :: MushroomId -> HyphalId -> Spore -> Price -> (MushroomBody, [HyphalTip])
germinateColony mid (HyphalId startAid) spore currentPrice =
    let
        genes = sporeGenome spore
        loc = sporeTarget spore
        (Capital totalCap) = sporeCapital spore
        nChildren = max 1 (geneMaxChildren genes)
        divisor = fromIntegral nChildren + 1.0
        shareSize = totalCap / divisor
        newMushroom = MushroomBody { mushId = mid, mushLocation = loc, mushMass = Capital shareSize, mushGenome = genes }
        createWorker i = HyphalTip
            { hypId = HyphalId (startAid + i), hypParentId = mid, hypLocation = loc, hypVelocity = [0,0], hypPath = [loc], hypHoldings = mempty
            , hypBiology = BioState { bioAge = 0, bioBank = Capital shareSize }, hypGenome = genes, hypRefPrice = currentPrice, hypStepCount = 0 }
        newHyphae = map createWorker [0..(nChildren - 1)]
    in (newMushroom, newHyphae)