module Main where

import MycelialState
import MycelialSimulation
-- CHANGED: Qualified import to prevent ambiguity
import qualified Control.Monad.State.Strict as Strict
import Text.Printf (printf)

-- ==========================================
-- 1. DATA GENERATORS
-- ==========================================

generateBullRun :: Int -> [Price]
generateBullRun steps = 
    [Price (100.0 + (100.0 * fromIntegral t / fromIntegral steps)) | t <- [0..steps]]

generateBearMarket :: Int -> [Price]
generateBearMarket steps = 
    [Price (100.0 - (50.0 * fromIntegral t / fromIntegral steps)) | t <- [0..steps]]

generateVolatility :: Int -> [Price]
generateVolatility steps = 
    [Price (100.0 + 10.0 * sin (fromIntegral t / 10.0)) | t <- [0..steps]]

-- ==========================================
-- 2. REPORTING HELPERS
-- ==========================================

calculateTVL :: SystemState -> Double
calculateTVL state =
    let 
        -- Unwrap GlobalWallet -> Capital -> Double
        (GlobalWallet (Capital w)) = sysWallet state
        (Price p) = mktPrice (sysEnv state)
        
        agentVal = sum $ map (\a -> 
            let (Quantity q) = posQuantity (hypHoldings a)
            in q * p + (case bioBank (hypBiology a) of Capital c -> c)
            ) (sysHyphae state)
            
        mushVal = sum $ map (\m -> case mushMass m of Capital c -> c) (sysMushrooms state)
        
        sporeVal = sum $ map (\s -> case sporeCapital s of Capital c -> c) (sysSpores state)
    in
        w + agentVal + mushVal + sporeVal

printStats :: String -> SystemState -> IO ()
printStats label state = do
    let tvl = calculateTVL state
    let hCount = length (sysHyphae state)
    let mCount = length (sysMushrooms state)
    let sCount = length (sysSpores state)
    
    let (GlobalWallet (Capital w)) = sysWallet state
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
    
    let simulation = mapM_ stepSimulation prices
    
    -- Explicitly use Strict.execState to match Sim monad
    let finalState = Strict.execState simulation genesisState
    
    putStrLn "--- FINAL RESULTS ---"
    printStats "End State" finalState
    
    case sysMushrooms finalState of
        [] -> putStrLn "No Mushrooms formed."
        ms -> do
            putStrLn "\n--- MUSHROOM DETAILS ---"
            mapM_ (\m -> 
                let mid = fromIntegral (mushId m) :: Int
                in printf "ID: %d | Mass: %.2f | Loc: %s\n" 
                    mid 
                    (case mushMass m of Capital c -> c) 
                    (show (mushLocation m))) ms

-- ==========================================
-- 4. MAIN
-- ==========================================

main :: IO ()
main = do
    putStrLn "Mycelial Model: Market Regime Stress Test"
    
    let duration = 500
    
    runScenario "BULL RUN (Uptrend)" (generateBullRun duration)
    runScenario "CRYPTO WINTER (Downtrend)" (generateBearMarket duration)
    runScenario "VOLATILITY (Sine Wave)" (generateVolatility duration)
    
    putStrLn "\nTest Complete."