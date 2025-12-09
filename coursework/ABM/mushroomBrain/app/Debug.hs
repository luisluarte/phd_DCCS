module Main where

import MycelialState
import MycelialSimulation
import Control.Monad.State
import Text.Printf (printf)
import Debug.Trace (trace)

-- A minimal test environment
debugGenesis :: SystemState
debugGenesis = genesisState 

main :: IO ()
main = do
    putStrLn "--- DEBUG: SINGLE TICK ANALYSIS ---"
    
    let state0 = debugGenesis
    let (Price p0) = mktPrice (sysEnv state0)
    printf "TICK 0: Agents=%d, Mushrooms=%d, Spores=%d, Price=%.2f\n" 
           (length $ sysHyphae state0) (length $ sysMushrooms state0) (length $ sysSpores state0) p0

    -- Run ONE tick
    let state1 = execState (stepSimulation (Price 101.0)) state0
    
    putStrLn "\n--- TICK 1 RESULT ---"
    let (Price p1) = mktPrice (sysEnv state1)
    printf "AGENTS: %d\n" (length $ sysHyphae state1)
    printf "MUSHROOMS: %d\n" (length $ sysMushrooms state1)
    printf "SPORES: %d\n" (length $ sysSpores state1)
    
    -- Deep dive into the mushroom
    case sysMushrooms state1 of
        [] -> putStrLn "Mushroom DIED."
        (m:_) -> do
            let (Capital mass) = mushMass m
            printf "Mushroom Mass: %.2f\n" mass
            
    -- Check if any agents survived
    case sysHyphae state1 of
        [] -> putStrLn "ALL AGENTS DIED."
        as -> putStrLn $ "Survivor Count: " ++ show (length as)