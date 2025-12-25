module Simulation.Loop where

import MycelialState
import MycelialPhysics (euclideanDistance)
import Simulation.Types (Sim) 
import Simulation.Accessors hiding (Sim)
import Simulation.Lifecycle (updateHypha, updateMushroom, germinateColony)
import Simulation.Macro (calculateLocalField, sensingRadius) 
import Simulation.Evolution (randomizeGenome)
import Control.Monad.State (modify, execState)
import System.Random (mkStdGen)
import Data.List (partition, foldl')
import Data.Maybe (catMaybes)

-- Filter spores that are too close to existing mushrooms (Density Dependent Inhibition)
filterCrowded :: [Spore] -> [MushroomBody] -> Double -> [Spore]
filterCrowded candidates mushrooms radius =
    filter (\s -> 
        let loc = sporeTarget s
            isCrowded = any (\m -> euclideanDistance loc (mushLocation m) < radius) mushrooms
        in not isCrowded
    ) candidates

stepSimulation :: Price -> Sim ()
stepSimulation newPrice = do
    agents <- getAgents
    mushrooms <- getMushrooms
    spores <- getSpores
    time <- getTime
    
    modify $ \s -> s { sysEnv = (sysEnv s) { mktPrice = newPrice } }
    
    -- 1. AGENT PASS
    results <- mapM (updateHypha newPrice mushrooms agents) agents
    let survivingAgents = catMaybes (map fst results)
    let allTaxes = concat (map snd results)

    -- 2. MUSHROOM PASS
    let (Time tInt) = time
    let processMushroom (mList, sList, recycledTotal) m =
          let 
             (MushroomId midInt) = mushId m
             seed = midInt + tInt
             rng = mkStdGen seed
             (mNew, newSpores, maintPaid) = updateMushroom newPrice allTaxes rng m
          in 
             if mushMass mNew > 0
             then (mNew : mList, newSpores ++ sList, recycledTotal + maintPaid)
             else 
                let deadMass = mushMass mNew
                in (mList, sList, recycledTotal + maintPaid + max (Capital 0.0) deadMass)
    
    let (livingMushrooms, newlyReleasedSpores, totalRecycled) = foldl' processMushroom ([], [], Capital 0.0) mushrooms
    
    modifyWallet (\c -> c + totalRecycled)

    -- 3. CASCADE DEATH
    let survivorIds = [mushId m | m <- livingMushrooms]
    let (orphans, keptAgents) = partition (\a -> not ((hypParentId a) `elem` survivorIds)) survivingAgents
    
    let orphanCash = sum [c | a <- orphans, let (Capital c) = bioBank (hypBiology a)]
    modifyWallet (\c -> c + Capital orphanCash)

    -- 4. SPORE PASS
    let allSpores = spores ++ newlyReleasedSpores

    -- UPDATED: Pass mushrooms to checkQuorum field calc
    let checkQuorum s =
          let
              loc = sporeTarget s
              -- FIXED: Include mushrooms in the field calculation
              field = calculateLocalField loc keptAgents livingMushrooms newPrice
              threshold = genePhiCritical (sporeGenome s)
          in
              field > threshold

    let (potentialColonizers, failures) = partition checkQuorum allSpores

    let exclusionRadius = sensingRadius * 0.25 
    
    let colonizers = filterCrowded potentialColonizers livingMushrooms exclusionRadius

    let crowdedOut = filter (\s -> not (s `elem` colonizers)) potentialColonizers
    let totalFailures = failures ++ crowdedOut

    let recycleAmount = sum [c | (Spore _ _ (Capital c)) <- totalFailures]
    let fedMushrooms = if not (null livingMushrooms) && recycleAmount > 0
          then
              let
                  share = recycleAmount / fromIntegral (length livingMushrooms)
                  feed m = m { mushMass = mushMass m + Capital share }
              in
                  map feed livingMushrooms
          else livingMushrooms

    let maxMid = if null fedMushrooms then 0 else maximum [i | (MushroomBody (MushroomId i) _ _ _) <- fedMushrooms]
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

    -- 5. COMMIT
    setSpores []
    setMushrooms (fedMushrooms ++ newColonies)
    setAgents (keptAgents ++ newWorkers)

    modify $ \s -> s { sysTime = sysTime s + 1 }


genesisState :: SystemState
genesisState = SystemState
    { sysTime      = Time 0
    , sysWallet    = GlobalWallet 10000.0
    , sysEnv       = Environment (Price 100.0) [] 
    , sysHyphae    = initialAgents 
    , sysMushrooms = [genesisMushroom]
    , sysSpores    = []
    }
  where
    genesisGenome = Genome
        {
        geneGreed = 0.5,
        geneTurbulence = 2.0,
        geneGrowthRate = 0.01,
        
        geneMaturity = 500.0,
        geneDispersion = 0.25,
        geneMaintenance = 0.2,

        genePhiCritical = 1.0,
        geneVacuumCoefficient = 0.2,
        geneReproductiveInvest = 0.2,
        geneSporeBatchSize = 5,

        geneBaseOrder = 200.0,
        geneDCAOrder = 200.0,
        geneMaxOrders = 5,
        geneDevMult = 1.0,
        geneVolMult = 1.0,
        geneMaxChildren = 5
        }

    genesisMushroom = MushroomBody
        { mushId = MushroomId 1
        , mushLocation = [0.03, 0.05] 
        , mushMass = Capital 1000.0
        , mushGenome = genesisGenome
        }

    initialAgents = 
        [ HyphalTip
            { hypId = HyphalId i
            , hypParentId = MushroomId 1
            , hypLocation = [0.03 + (fromIntegral i * 0.001), 0.05 + (fromIntegral i * 0.001)] 
            , hypVelocity = [0.001 * fromIntegral (i `mod` 3 - 1), 0.001 * fromIntegral (i `mod` 2 - 1)]
            , hypPath     = [[0.03, 0.05]]
            , hypHoldings = Position (Quantity 0.0) (Capital 0.0)
            , hypBiology  = BioState 0 (Capital 200.0)
            , hypGenome   = genesisGenome
            , hypRefPrice = Price 100.0
            , hypStepCount = 0
            }
        | i <- [1..10]
        ] 