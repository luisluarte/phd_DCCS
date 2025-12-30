module Simulation.Loop where

import MycelialState
import MycelialPhysics (calculateNutrient)
import System.Random (mkStdGen, randomR, StdGen)
import Data.List (mapAccumL)

-- | MOVEMENT & FEEDING
updateAgent :: SimConfig -> [Double] -> Int -> StdGen -> HyphalTip -> (StdGen, HyphalTip)
updateAgent cfg history tNow rng agent =
    let 
        -- 1. MOVE (Random Walk)
        (theta, r1) = randomR (0.0, 2.0 * pi) rng
        (step, r2)  = randomR (0.0, cfgMaxStep cfg) r1
        
        (x, y) = hypLoc agent
        dx = step * cos theta
        dy = step * sin theta
        
        -- Clamp to world (Lags must be positive integers logically)
        maxL = fromIntegral (cfgMaxLag cfg)
        newX = max 1.0 (min maxL (x + dx))
        newY = max 1.0 (min maxL (y + dy))
        
        -- 2. FEED (Sample the Nutrient Field at integer coordinates)
        lag1 = round newX
        lag2 = round newY
        
        nutrient = calculateNutrient history tNow lag1 lag2 (cfgSimilarityThreshold cfg)
        
        (Capital currentMass) = hypBiomass agent
        -- Simple metabolism: Gain nutrient, lose tiny maintenance
        newMass = currentMass + nutrient - 0.01 
        
    in (r2, agent { hypLoc = (newX, newY), hypBiomass = Capital (max 0.0 newMass) })

-- | MAIN STEP
stepSimulation :: SimConfig -> Double -> SystemState -> SystemState
stepSimulation cfg newPrice state =
    let
        t = sysTime state
        history = sysHistory state ++ [newPrice]
        
        seed = t * 999
        rng = mkStdGen seed
        
        -- Update all agents
        (_, newAgents) = mapAccumL 
            (\r ag -> updateAgent cfg history t r ag) 
            rng 
            (sysAgents state)

    in state
        { sysTime    = t + 1
        , sysHistory = history
        , sysAgents  = newAgents
        }

-- | GENESIS
genesisState :: SimConfig -> [Double] -> SystemState
genesisState cfg p0 = 
    let 
        n = cfgNumAgents cfg
        maxL = fromIntegral (cfgMaxLag cfg)
        rng = mkStdGen 123
        
        createAgent (r, i) = 
            let (x, r1) = randomR (1.0, maxL) r
                (y, r2) = randomR (1.0, maxL) r1
            in (r2, HyphalTip i (x, y) (Capital 1.0))
            
        (_, agents) = mapAccumL (\r i -> createAgent (r, i)) rng [1..n]
        
    in SystemState
        { sysTime    = length p0
        , sysHistory = p0
        , sysAgents  = agents
        }
