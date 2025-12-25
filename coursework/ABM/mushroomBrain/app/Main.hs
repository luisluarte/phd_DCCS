{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Import the unified simulation module
import MycelialSimulation 
-- Import state definitions to inspect data structures
import MycelialState
-- Import strategy and physics for deep inspection if needed
import MycelialStrategy
import MycelialPhysics

import Control.Monad.State
import Text.Printf (printf)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))

-- ==========================================
-- REPL HELPER FUNCTIONS
-- ==========================================

-- | Initialize a fresh simulation state
initState :: SystemState
initState = genesisState

-- | Step the simulation forward by one tick with a given price
-- Usage in REPL: 
-- let s1 = step (Price 100.0) initState
-- let s2 = step (Price 101.0) s1
step :: Price -> SystemState -> SystemState
step p s = execState (stepSimulation p) s

-- | Run a sequence of prices and return the final state
-- Usage: let final = runSeq [Price 100, Price 105, Price 95] initState
runSeq :: [Price] -> SystemState -> SystemState
runSeq prices startState = foldl (flip step) startState prices

-- | Print a summary of the current state to the console
inspect :: SystemState -> IO ()
inspect s = do
    let (Time t) = sysTime s
    let (GlobalWallet (Capital w)) = sysWallet s
    let (Price p) = mktPrice (sysEnv s)
    let agents = sysHyphae s
    let mushrooms = sysMushrooms s
    let spores = sysSpores s
    
    putStrLn $ "--- State at Tick " ++ show t ++ " ---"
    printf "Market Price  : %.2f\n" p
    printf "Global Wallet : %.2f\n" w
    printf "Population    : %d Hyphae, %d Mushrooms, %d Spores\n" (length agents) (length mushrooms) (length spores)
    
    -- Print details of first few mushrooms if any
    case mushrooms of
        [] -> putStrLn "No Mushrooms."
        (m:_) -> do
            let (MushroomId mid) = mushId m
            let (Capital mass) = mushMass m
            printf "Mushroom #%d Mass: %.2f\n" mid mass

-- ==========================================
-- MAIN ENTRY POINT
-- ==========================================

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Mycelial REPL Environment Loaded."
    putStrLn "Use 'cabal repl' to interact."
    putStrLn "Available commands:"
    putStrLn "  initState       - Get a fresh genesis state"
    putStrLn "  step p s        - Advance state 's' with price 'p'"
    putStrLn "  runSeq ps s     - Advance state 's' with price list 'ps'"
    putStrLn "  inspect s       - Print summary of state 's'"
    
    -- Example usage in compiled run:
    let s0 = initState
    inspect s0