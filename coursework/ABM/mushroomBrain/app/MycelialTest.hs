module Main where

import MycelialState
import MycelialSimulation
import Control.Monad.State
import Text.Printf (printf)

-- ==========================================
-- 1. DATA GENERATORS (Synthetic Time Series)
-- ==========================================

-- Scenario A: Bull Market (Linear Uptrend)
-- Price moves from 100.0 to 200.0
generateBullRun :: Int -> [Price]
generateBullRun steps = 
    [Price (100.0 + (100.0 * fromIntegral t / fromIntegral steps)) | t <- [0..steps]]

-- Scenario B: Bear Market (Linear Downtrend)
-- Price moves from 100.0 to 50.0 (50% Drop)
generateBearMarket :: Int -> [Price]
generateBearMarket steps = 
    [Price (100.0 - (50.0 * fromIntegral t / fromIntegral steps)) | t <- [0..steps]]

-- Scenario C: Volatile Chop (Sine Wave)
-- Oscillates between 90 and 110
generateVolatility :: Int -> [Price]
generateVolatility steps = 
    [Price (100.0 + 10.0 * sin (fromIntegral t / 10.0)) | t <- [0..steps]]

-- ==========================================
-- 2. REPORTING HELPERS
-- ==========================================

-- Calculate Total System Value (TVL)
-- TVL = Wallet + (Agent Inventory * Price) + (Mushroom Mass) + (Spore Capital)
calculateTVL :: SystemState -> Double
calculateTVL state =
    let 
        (GlobalWallet w) = sysWallet state
        (Price p) = mktPrice (sysEnv state)
        
        -- Agent Value
        agentVal = sum $ map (\a -> 
            let (Quantity q) = posQuantity (hypHoldings a)
            in q * p + (case bioBank (hypBiology a) of Capital c -> c)
            ) (sysHyphae state)
            
        -- Mushroom Value
        mushVal = sum $ map (\m -> case mushMass m of Capital c -> c) (sysMushrooms state)
        
        -- Spore Value
        sporeVal = sum $ map (\s -> case sporeCapital s of Capital c -> c) (sysSpores state)
    in
        w + agentVal + mushVal + sporeVal

printStats :: String -> SystemState -> IO ()
printStats label state = do
    let tvl = calculateTVL state
    let hCount = length (sysHyphae state)
    let mCount = length (sysMushrooms state)
    let sCount = length (sysSpores state)
    let (GlobalWallet w) = sysWallet state
    let (Price p) = mktPrice (sysEnv state)
    
    printf "%-15s | Price: %6.2f | TVL: %9.2f | Wallet: %8.2f | Agents: %3d | Mushrooms: %3d | Spores: %3d\n" 
           label p tvl w hCount mCount sCount

-- ==========================================
-- 3. TEST RUNNER
-- ==========================================

runScenario :: String -> [Price] -> IO ()
runScenario name prices = do
    putStrLn $ "\n========================================"
    putStrLn $ "SCENARIO: " ++ name
    putStrLn $ "Duration: " ++ show (length prices) ++ " ticks"
    putStrLn "========================================"
    
    -- Run Simulation
    -- We map stepSimulation over the price list
    let simulation = mapM_ stepSimulation prices
    
    -- Execute State Monad starting from Genesis
    let finalState = execState simulation genesisState
    
    -- Report Results
    putStrLn "--- FINAL RESULTS ---"
    printStats "End State" finalState
    
    -- Show breakdown of Mushrooms if any exist
    case sysMushrooms finalState of
        [] -> putStrLn "No Mushrooms formed."
        ms -> do
            putStrLn "\n--- MUSHROOM DETAILS ---"
            mapM_ (\m -> printf "ID: %d | Mass: %.2f | Loc: %s\n" 
                    (mushId m) 
                    (case mushMass m of Capital c -> c) 
                    (show (mushLocation m))) ms

-- ==========================================
-- 4. MAIN
-- ==========================================

main :: IO ()
main = do
    putStrLn "Mycelial Model: Market Regime Stress Test"
    
    -- 500 Ticks for each scenario
    let duration = 500
    
    -- 1. Run Bull
    runScenario "BULL RUN (Uptrend)" (generateBullRun duration)
    
    -- 2. Run Bear
    runScenario "CRYPTO WINTER (Downtrend)" (generateBearMarket duration)
    
    -- 3. Run Volatility
    runScenario "VOLATILITY (Sine Wave)" (generateVolatility duration)
    
    putStrLn "\nTest Complete."