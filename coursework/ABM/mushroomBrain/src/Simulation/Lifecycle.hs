module Simulation.Lifecycle where

import MycelialState
import Simulation.Types (Sim)
import Simulation.Accessors
import MycelialPhysics (moveHypha)
import qualified Data.Map.Strict as Map

-- Logic to update a single Hypha
updateHypha :: Bool -> Price -> Map.Map MushroomId (MushroomBody, Double) -> [HyphalTip] -> HyphalTip -> Sim (Maybe HyphalTip, [((MushroomId, MushroomId), Capital)], Capital)
updateHypha intelligenceEnabled currentPrice mushMap allAgents agent = do
    let genes = hypGenome agent
        (Price p) = currentPrice
        (Capital bank) = bioBank (hypBiology agent)
        (Quantity qty) = posQuantity (hypHoldings agent)
        (Price avgEntry) = hypAvgEntry agent
        maintCost = Capital (geneMaintenance genes)

    -- 1. Physiological Tax (Maintenance)
    let bankAfterMaint = bank - (case maintCost of Capital c -> c)
    
    if bankAfterMaint <= 0 
    then return (Nothing, [], 0) -- Agent dies
    else do
        -- 2. Movement logic
        let newLoc = moveHypha intelligenceEnabled currentPrice mushMap allAgents agent
        
        -- 3. TRADING LOGIC (Weighted Average Cost Basis)
        let 
            -- HURDLE: Only sell if current price is > Average Entry * (1 + Greed)
            shouldSell = qty > 0 && p > (avgEntry * (1 + geneGreed genes))
            
            -- DCA BUY: Only buy if we have cash and price is below average (or if first buy)
            shouldBuy = bankAfterMaint > geneBaseOrder genes && (avgEntry == 0 || p < avgEntry)

        (finalBank, finalHoldings, finalAvgEntry, vacuumTax) <- case () of
            _ | shouldSell -> do
                let saleProceeds = qty * p
                    -- Vacuum takes a cut of the TRANSACTION value if enabled
                    tax = saleProceeds * geneVacuumCoefficient genes
                    netProceeds = saleProceeds - tax
                return (bankAfterMaint + netProceeds, Position 0, Price 0, Capital tax)
                
            _ | shouldBuy -> do
                let buySpend = geneBaseOrder genes
                    boughtQty = buySpend / p
                    newTotalQty = qty + boughtQty
                    -- WEIGHTED AVERAGE FORMULA: (OldTotalVal + NewTotalVal) / NewTotalQty
                    newAvgEntry = ((qty * avgEntry) + (boughtQty * p)) / newTotalQty
                return (bankAfterMaint - buySpend, Position (Quantity newTotalQty), Price newAvgEntry, 0)
                
            _ -> return (bankAfterMaint, hypHoldings agent, Price avgEntry, 0)

        -- 4. Construct updated agent
        let updatedAgent = agent 
                { hypLocation = newLoc
                , hypPath     = newLoc : hypPath agent
                , hypHoldings = finalHoldings
                , hypAvgEntry = finalAvgEntry
                , hypBiology  = (hypBiology agent) { bioBank = Capital finalBank }
                , hypStepCount = hypStepCount agent + 1
                }
        
        -- Vacuum tax is returned as "Taxes" to be redistributed in Loop.hs
        let taxes = if vacuumTax > 0 
                    then [((hypParentId agent, hypParentId agent), vacuumTax)] 
                    else []
                    
        return (Just updatedAgent, taxes, maintCost)

-- Mushroom & Colony functions (Simplified versions, assuming logic is handled in Loop.hs)
updateMushroom :: Bool -> Price -> [((MushroomId, MushroomId), Capital)] -> r -> MushroomBody -> (MushroomBody, [Spore], Capital)
updateMushroom mutationEnabled p taxes rng m = 
    let genes = mushGenome m
        maint = Capital (geneMaintenance genes)
        (Capital currentMass) = mushMass m
        -- Sum taxes assigned to this mushroom
        income = sum [amt | ((_, target), amt) <- taxes, target == mushId m]
        newMass = currentMass + (case income of Capital i -> i) - (case maint of Capital c -> c)
    in (m { mushMass = Capital newMass }, [], maint)

germinateColony :: MushroomId -> HyphalId -> Spore -> Price -> (MushroomBody, [HyphalTip])
germinateColony mid hid spore p = 
    let m = MushroomBody mid (sporeTarget spore) (sporeCapital spore / 2) (sporeGenome spore)
        a = HyphalTip hid mid (sporeTarget spore) [0,0] [sporeTarget spore] (Position 0) (BioState 0 (sporeCapital spore / 2)) (sporeGenome spore) p 0
    in (m, [a])
