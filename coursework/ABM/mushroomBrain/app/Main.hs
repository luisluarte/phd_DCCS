{-# LANGUAGE OverloadedStrings #-}
module Main where

import MycelialState
import MycelialSimulation (runSeq)
import Simulation.Loop (genesisState) 
import Simulation.Evolution (randomizeGenome)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as B
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Environment (getArgs)

-- ============================================================================
-- 1. CUSTOM GENESIS INITIALIZATION
-- ============================================================================
makeCustomGenesis :: SimConfig -> SystemState
makeCustomGenesis cfg =
    let 
        -- 1. Construct the Base Genome from R input
        customGenome = Genome
            { geneGreed             = cfgInitGreed cfg
            , geneTurbulence        = cfgInitTurbulence cfg
            , geneGrowthRate        = cfgInitGrowthRate cfg
            , geneBaseOrder         = cfgInitBaseOrder cfg
            , genePhiCritical       = cfgInitPhiCritical cfg
            , geneReproductiveInvest= cfgInitReproductiveInvest cfg
            , geneVacuumCoefficient = cfgInitVacuumCoefficient cfg
            , geneDevMult           = cfgInitDevMult cfg
            
            -- Fixed / System Parameters mapped from Config
            , geneSporeBatchSize    = cfgSporeBatchSize cfg
            , geneDCAOrder          = cfgDcaOrder cfg
            , geneMaxOrders         = cfgMaxOrders cfg
            , geneMaxChildren       = cfgMaxChildren cfg
            , geneDispersion        = cfgDispersionRadius cfg
            , geneMaintenance       = cfgMaintenanceCost cfg
            , geneMaturity          = cfgInitMaturity cfg
            , geneVolMult           = 1.0
            }

        -- 2. Helper to update a Mushroom's genome
        updateMush m = m { mushGenome = customGenome }
        
        -- 3. Helper to update a Hypha's genome WITH RANDOMIZATION
        updateHyp h = 
            let (HyphalId i) = hypId h
            in h { hypGenome = randomizeGenome customGenome (i * 9999) }
        
        -- 4. Get the default state
        -- FIXED: Pass cfgMaxChildren to genesisState so initial population matches config
        s0 = genesisState (cfgMaxChildren cfg)
    in 
        -- 5. Return state with updated agents
        s0
        { sysMushrooms = map updateMush (sysMushrooms s0)
        , sysHyphae    = map updateHyp (sysHyphae s0)
        }

-- ============================================================================
-- 2. PIPELINE MODE
-- ============================================================================

runPipelineMode :: IO ()
runPipelineMode = do
    inputRaw <- B.getContents
    let payload = decode inputRaw :: Maybe InputPayload
    
    case payload of
        Nothing -> error "JSON Decoding Failed: Check input format in R."
        Just (InputPayload pricesRaw config) -> do
            
            let s0 = makeCustomGenesis config
            let prices = map Price pricesRaw
            let finalState = runSeq config prices s0
            let stats = reverse (sysSnapshots finalState)
            let equity = map statTotalWealth stats
            
            let output = OutputPayload 
                  { outputEquityCurve = equity
                  , outputStats = stats 
                  }
            
            B.putStr (encode output)

-- ============================================================================
-- 3. ENTRY POINT
-- ============================================================================

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
        ["--pipeline"] -> runPipelineMode
        _              -> putStrLn "Usage: mycelial-exe --pipeline < input.json"
