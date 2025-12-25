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


printTradeLog :: SystemState -> IO ()
printTradeLog s = do
    let logs = reverse (sysLogs s) 
    putStrLn "\n=== TRANSACTION LOG ==="
    -- ADDED: Tick column header
    putStrLn "Tick | HyphaID | Action     | Cost/Rev  | Price   | Qty"
    putStrLn "-----------------------------------------------------------"
    mapM_ printLog logs
    putStrLn "-----------------------------------------------------------"
  where
    printLog :: TransactionLog -> IO ()
    printLog l = do
        let (Time t) = tlTime l  -- Extract Time
        let (HyphalId hid) = tlHyphaId l
        let action = show (tlType l)
        let (Capital c) = tlCost l
        let (Price p) = tlPrice l
        let (Quantity q) = tlQuantity l
        
        -- ADDED: 't' to printf
        printf "%-4d | %-7d | %-10s | %9.2f | %7.2f | %.4f\n" t hid action c p q

-- | Prints aggregate financial state over time (Mark-to-Market)
printPerformanceLog :: SystemState -> IO ()
printPerformanceLog s = do
    let snaps = reverse (sysSnapshots s)
    putStrLn "\n=== PERFORMANCE LOG (Equity Curve) ==="
    putStrLn "Time | MktPrice | Cash       | Stock    | MushMass | FractalDim | TOTAL WEALTH"
    putStrLn "----------------------------------------------------------------------------------"
    mapM_ printSnap snaps
    putStrLn "----------------------------------------------------------------------------------"
  where
    printSnap :: SystemSnapshot -> IO ()
    printSnap sn = do
        let (Time t) = snapTime sn
        let (Price p) = snapMarketPrice sn
        let (Capital c) = snapTotalCash sn
        let (Capital i) = snapInventoryValue sn
        let (Capital m) = snapMushroomMass sn
        let dim = snapMeanFractalDim sn
        let (Capital w) = snapTotalWealth sn
        
        -- Display Fractal Dimension with 4 decimal places
        printf "%-4d | %8.2f | %10.2f | %8.2f | %8.2f | %10.4f | %12.2f\n" t p c i m dim w


-- ==========================================
-- MAIN ENTRY POINT
-- ==========================================

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Mycelial REPL Environment Loaded."
    putStrLn "Use 'cabal repl' to interact."
    putStrLn "Available commands:"
    putStrLn "  initState           - Get a fresh genesis state"
    putStrLn "  step p s            - Advance state 's' with price 'p'"
    putStrLn "  runSeq ps s         - Advance state 's' with price list 'ps'"
    putStrLn "  inspect s           - Print summary of state 's'"
    putStrLn "  printTradeLog s     - Print transaction history"
    putStrLn "  printPerformanceLog s - Print equity curve history"
    
    let s0 = initState
    inspect s0