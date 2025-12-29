module Simulation.Loop where

import MycelialState
import qualified Simulation.Types as T
import MycelialPhysics (euclideanDistance, calculateFractalDim)
import Simulation.Types (Sim) 
import Simulation.Accessors hiding (Sim)
import Simulation.Lifecycle (updateHypha, updateMushroom, germinateColony)
import Simulation.Macro (calculateLocalField, sensingRadius, applyDrain) 
import Simulation.Evolution (randomizeGenome) -- ADDED THIS IMPORT
import Control.Monad.State (modify)
import System.Random (mkStdGen)
import Data.List (partition, foldl')
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map 

-- Filter spores that are too close to existing mushrooms
filterCrowded :: [Spore] -> [MushroomBody] -> Double -> [Spore]
filterCrowded candidates mushrooms radius =
    filter (\s -> 
        let loc = sporeTarget s
            isCrowded = any (\m -> euclideanDistance loc (mushLocation m) < radius) mushrooms
        in not isCrowded
    ) candidates

stepSimulation :: SimConfig -> Price -> T.Sim ()
stepSimulation config newPrice = do
    agents <- getAgents
    mushrooms <- getMushrooms
    spores <- getSpores
    time <- getTime
    
    modify $ \s -> s { sysEnv = (sysEnv s) { mktPrice = newPrice } }
    
    -- 1. PRE-CALCULATE MUSHROOM FIELDS
    let mushCache = Map.fromList 
          [ (mushId m, (m, calculateLocalField (mushLocation m) agents mushrooms newPrice)) 
          | m <- mushrooms 
          ]

    -- 2. AGENT PASS
    let intelligenceEnabled = cfgEnableIntelligence config

    -- updateHypha returns (Maybe Agent, Taxes, MaintenancePaid)
    results <- mapM (updateHypha intelligenceEnabled newPrice mushCache agents) agents
    
    let survivingAgents = [a | (Just a, _, _) <- results]
    
    -- IMPORTANT: Taxes are now calculated via Macro.applyDrain inside Loop usually, 
    -- but here we assume Lifecycle/Micro handles the trade/move, and we check taxes here 
    -- OR Lifecycle returns them.
    -- In your previous Macro.hs, applyDrain was a standalone function.
    -- If Lifecycle.updateHypha returns taxes (middle tuple element), we use them.
    let allTaxes = concat [t | (_, t, _) <- results]
    let totalAgentMaint = sum [m | (_, _, m) <- results]

    -- 3. MUSHROOM PASS
    let (Time tInt) = time
    let mutationEnabled = cfgEnableMutation config

    let processMushroom (mList, sList, recycledTotal) m =
          let 
             (MushroomId midInt) = mushId m
             seed = midInt + tInt
             rng = mkStdGen seed
             (mNew, newSpores, maintPaid) = updateMushroom mutationEnabled newPrice allTaxes rng m
          in 
             if mushMass mNew > 0
             then (mNew : mList, newSpores ++ sList, recycledTotal + maintPaid)
             else 
                let (Capital deadVal) = mushMass mNew
                in (mList, sList, recycledTotal + maintPaid + Capital (max 0.0 deadVal))
    
    let (livingMushroomsRaw, newlyReleasedSpores, mushroomRecycled) = 
            foldl' processMushroom ([], [], Capital 0.0) mushrooms
    
    -- 4. CASCADE DEATH & ASSET COLLECTION
    let survivorIds = [mushId m | m <- livingMushroomsRaw]
    let (orphans, keptAgents) = partition (\a -> not ((hypParentId a) `elem` survivorIds)) survivingAgents
    
    let orphanCash = sum [c | a <- orphans, let (Capital c) = bioBank (hypBiology a)]
    let orphanStockVal = sum [q * p | a <- orphans, 
                                let (Quantity q) = posQuantity (hypHoldings a), 
                                let (Price p) = newPrice]
    
    let totalRecyclePool = mushroomRecycled + totalAgentMaint + Capital (orphanCash + orphanStockVal)

    -- 5. REDISTRIBUTE TO SURVIVING MUSHROOMS
    let livingCount = length livingMushroomsRaw
    let livingMushrooms = if livingCount > 0 && totalRecyclePool > 0
          then
              let 
                  share = totalRecyclePool / fromIntegral livingCount
                  feed m = m { mushMass = mushMass m + share }
              in map feed livingMushroomsRaw
          else livingMushroomsRaw

    -- 6. SPORE PASS
    let allSpores = spores ++ newlyReleasedSpores
    
    -- 7. SNAPSHOT & STATS
    (GlobalWallet wCap) <- getWallet 
    let (Capital w) = wCap 
    let (Price p) = newPrice
    let agentCash = sum [c | a <- keptAgents, let (Capital c) = bioBank (hypBiology a)]
    let agentStockVal = sum [q * p | a <- keptAgents, let (Quantity q) = posQuantity (hypHoldings a)]
    let mushVal = sum [m | mBody <- livingMushrooms, let (Capital m) = mushMass mBody]
    let sporeVal = sum [s | sp <- allSpores, let (Capital s) = sporeCapital sp]
    
    let currentGenomes = map hypGenome keptAgents

    let locations = map hypLocation keptAgents
    let stratDrops = map (\loc -> (if null loc then 0.0 else loc !! 0) * 0.05) locations
    let stratProfits = map (\loc -> (if length loc < 2 then 0.0 else loc !! 1) * 0.05) locations

    let snapshot = SimStats 
          { statTick = tInt + 1
          , statTotalWealth = agentCash + agentStockVal + mushVal + sporeVal
          , statMktPrice = p
          , statPopSize = length keptAgents
          , statFractalDims = map (calculateFractalDim . hypPath) keptAgents
          , statHoldings    = map (\a -> let (Quantity q) = posQuantity (hypHoldings a) in q) keptAgents
          , statBioBank     = map (\a -> let (Capital c) = bioBank (hypBiology a) in c) keptAgents
          , statGeneGreed              = map geneGreed currentGenomes
          , statGeneTurbulence         = map geneTurbulence currentGenomes
          , statGeneGrowthRate         = map geneGrowthRate currentGenomes
          , statGeneBaseOrder          = map geneBaseOrder currentGenomes
          , statGenePhiCritical        = map genePhiCritical currentGenomes
          , statGeneReproductiveInvest = map geneReproductiveInvest currentGenomes
          , statGeneVacuumCoefficient  = map geneVacuumCoefficient currentGenomes
          , statGeneDevMult            = map geneDevMult currentGenomes
          , statStratDrop   = stratDrops
          , statStratProfit = stratProfits
          }
    modify $ \s -> s { sysSnapshots = snapshot : sysSnapshots s }

    -- 8. COLONIZATION
    let checkQuorum s =
          let
              loc = sporeTarget s
              field = calculateLocalField loc keptAgents livingMushrooms newPrice
              threshold = genePhiCritical (sporeGenome s)
          in
              field > threshold

    let (potentialColonizers, failures) = partition checkQuorum allSpores
    let exclusionRadius = sensingRadius * 0.25 
    let colonizers = filterCrowded potentialColonizers livingMushrooms exclusionRadius
    let crowdedOut = filter (\s -> not (s `elem` colonizers)) potentialColonizers
    let totalFailures = failures ++ crowdedOut

    let sporeRecycleAmount = sum [c | (Spore _ _ (Capital c)) <- totalFailures]
    let finalMushrooms = if not (null livingMushrooms) && sporeRecycleAmount > 0
          then
              let
                  share = sporeRecycleAmount / fromIntegral (length livingMushrooms)
                  feed m = m { mushMass = mushMass m + Capital share }
              in
                  map feed livingMushrooms
          else livingMushrooms

    let maxMid = if null finalMushrooms then 0 else maximum [i | (MushroomBody (MushroomId i) _ _ _) <- finalMushrooms]
    let maxHid = if null keptAgents then 0 else maximum [i | (HyphalTip (HyphalId i ) _ _ _ _ _ _ _ _ _) <- keptAgents]

    let startMid = maxMid + 1
    let startHid = maxHid + 1

    let processSpore (mList, aList, nextMid, nextHid) spore =
          let
              (newM, newAs) = germinateColony (MushroomId nextMid) (HyphalId nextHid) spore newPrice
              count = length newAs
          in
              (newM : mList, newAs ++ aList, nextMid + 1, nextHid + count)

    let (newColonies, newWorkers, _, _) = 
            foldl' processSpore ([], [], startMid, startHid) colonizers

    -- 9. COMMIT STATE
    setSpores [] 
    setMushrooms (finalMushrooms ++ newColonies)
    setAgents (keptAgents ++ newWorkers)

    modify $ \s -> s { sysTime = sysTime s + 1 }

-- GENESIS STATE
genesisState :: SystemState
genesisState = SystemState
    { sysTime      = Time 0
    , sysWallet    = GlobalWallet 100.0
    , sysEnv       = Environment (Price 1.0) []
    , sysHyphae    = initialAgents 
    , sysMushrooms = [genesisMushroom]
    , sysSpores    = []
    , sysLogs      = []
    , sysSnapshots = []
    }
  where
    genesisGenome = Genome
        {
        geneGreed = 0.5,
        geneTurbulence = 2.0,
        geneGrowthRate = 0.01,
        geneMaturity = 50.0,
        geneDispersion = 0.25,
        geneMaintenance = 0.001,
        genePhiCritical = 1.0,
        geneVacuumCoefficient = 0.2,
        geneReproductiveInvest = 0.2,
        geneSporeBatchSize = 5,
        geneBaseOrder = 10.0,
        geneDCAOrder = 10.0,
        geneMaxOrders = 10,
        geneDevMult = 1.0,
        geneVolMult = 1.0,
        geneMaxChildren = 5
        }

    genesisMushroom = MushroomBody
        { mushId = MushroomId 1
        , mushLocation = [0.5, 0.5]
        , mushMass = Capital 100.0
        , mushGenome = genesisGenome
        }

    -- HIGH-ENTROPY INITIALIZATION
    initialAgents = 
        [ HyphalTip
            { hypId = HyphalId i
            , hypParentId = MushroomId 1
            , hypLocation = [0.5 + (dx * 0.01), 0.5 + (dy * 0.01)] 
            , hypVelocity = [dx * 0.001, dy * 0.001]
            , hypPath     = [[0.5, 0.5]]
            , hypHoldings = mempty
            , hypBiology  = BioState 0 (Capital 100.0)
            
            -- FIX: Apply randomization here as requested
            , hypGenome   = randomizeGenome genesisGenome (i * 1337)
            
            -- FIX: Correct field name from hypAvgEntry to hypRefPrice
            , hypRefPrice = Price 1.0
            
            , hypStepCount = 0
            }
        | i <- [1..5]
        , let angle = (fromIntegral i / 5.0) * 2 * pi
        , let dx = cos angle
        , let dy = sin angle
        ]
