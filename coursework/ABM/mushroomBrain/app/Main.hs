{-# LANGUAGE OverloadedStrings #-}
module Main where

import MycelialState
import MycelialSimulation (runSeq)
import Simulation.Loop (genesisState) 
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as B
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Environment (getArgs)

-- ============================================================================
-- 1. CUSTOM GENESIS INITIALIZATION
-- ============================================================================
-- This function overwrites the default 'genesisState' with the specific 
-- parameters passed from R (via SimConfig).

makeCustomGenesis :: SimConfig -> SystemState
makeCustomGenesis cfg =
    let 
        -- 1. Construct the Genome based on R input
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
            
            -- Defaults (Not exposed to R in this iteration, keeping hardcoded defaults)
            , geneMaturity          = 500.0 
            , geneVolMult           = 1.0
            }

        -- 2. Helper to update a Mushroom's genome
        updateMush m = m { mushGenome = customGenome }
        
        -- 3. Helper to update a Hypha's genome
        updateHyp h = h { hypGenome = customGenome }
        
        -- 4. Get the default state
        s0 = genesisState
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
    -- 1. Read Raw JSON from Stdin (passed by R)
    inputRaw <- B.getContents
    
    -- 2. Decode the InputPayload
    let payload = decode inputRaw :: Maybe InputPayload
    
    case payload of
        Nothing -> error "JSON Decoding Failed: Check input format in R."
        Just (InputPayload pricesRaw config) -> do
            
            -- 3. Initialize System with Custom Config
            let s0 = makeCustomGenesis config
            let prices = map Price pricesRaw
            
            -- 4. Run Simulation 
            -- We pass 'config' to runSeq so it can control Mutation/Intelligence logic
            let finalState = runSeq config prices s0
            
            -- 5. Extract Statistics (SimStats) from the History
            -- sysSnapshots is stored in reverse order, so we reverse it back
            let stats = reverse (sysSnapshots finalState)
            
            -- 6. Extract Equity Curve (Total Wealth) separately for easy plotting
            let equity = map statTotalWealth stats
            
            -- 7. Construct Output Payload
            let output = OutputPayload 
                  { outputEquityCurve = equity
                  , outputStats = stats 
                  }
            
            -- 8. Print JSON to Stdout
            B.putStr (encode output)

-- ============================================================================
-- 3. ENTRY POINT
-- ============================================================================

main :: IO ()
main = do
    -- Ensure stdout doesn't buffer, so R gets data immediately if needed
    hSetBuffering stdout NoBuffering
    
    args <- getArgs
    case args of
        ["--pipeline"] -> runPipelineMode
        _              -> putStrLn "Usage: mycelial-exe --pipeline < input.json"