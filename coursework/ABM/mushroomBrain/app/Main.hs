{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import MycelialState
import MycelialSimulation hiding (main)
import MycelialStrategy (interpretStrategy, TradingStrategy(..))
import MycelialPhysics (calculateFractalDim) 
import MycelialMetrics (calculateStats, printPerformanceReport)
import SP500Data (loadSP500Data)

-- STRICT STATE IMPORTS
import Control.Monad.State.Strict
import Control.Monad (when, foldM)
import Control.Monad.IO.Class (liftIO)
import Text.Printf (printf)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))
import Text.Read (readMaybe)
import Data.List (sortBy, intercalate)
import Data.Ord (comparing)

-- Optimized Text Imports
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as BI
import qualified Data.Text.Lazy.Builder.RealFloat as BF

-- ==========================================
-- 1. DATA GENERATORS (Legacy Synthetic)
-- ==========================================

addNoise :: Int -> Double -> Double
addNoise t magnitude = magnitude * sin (fromIntegral t / 5.0)

generateBullRun :: Int -> [Price]
generateBullRun steps = 
    [Price (100.0 + (100.0 * fromIntegral t / fromIntegral steps) + addNoise t 2.0) | t <- [0..steps]]

generateBearMarket :: Int -> [Price]
generateBearMarket steps = 
    [Price (100.0 - (50.0 * fromIntegral t / fromIntegral steps) + addNoise t 2.0) | t <- [0..steps]]

generateVolatility :: Int -> [Price]
generateVolatility steps = 
    [Price (100.0 + 10.0 * sin (fromIntegral t / 10.0)) | t <- [0..steps]]

-- ==========================================
-- 2. REPORTING & TVL CALCULATION
-- ==========================================

calculateTVL :: SystemState -> Double
calculateTVL state =
    let (GlobalWallet (Capital w)) = sysWallet state
        (Price p) = mktPrice (sysEnv state)
        agentVal = sum $ map (\a -> let (Quantity q) = posQuantity (hypHoldings a) in q * p + (case bioBank (hypBiology a) of Capital c -> c)) (sysHyphae state)
        mushVal = sum $ map (\m -> case mushMass m of Capital c -> c) (sysMushrooms state)
        sporeVal = sum $ map (\s -> case sporeCapital s of Capital c -> c) (sysSpores state)
    in w + agentVal + mushVal + sporeVal

printReport :: String -> SystemState -> IO ()
printReport label state = do
    let tvl = calculateTVL state
    let hCount = length (sysHyphae state)
    let mCount = length (sysMushrooms state)
    let (GlobalWallet (Capital w)) = sysWallet state
    let (Price p) = mktPrice (sysEnv state)
    printf "\n--- %s REPORT ---\nMarket Price: %.2f\nTVL: %.2f\nWallet: %.2f\nAgents: %d | Mushrooms: %d\n" label p tvl w hCount mCount

printMushroomDetails :: SystemState -> IO ()
printMushroomDetails state = do
    case sysMushrooms state of
        [] -> putStrLn "No Mushrooms."
        ms -> do 
            let sortedMs = sortBy (flip $ comparing (\m -> case mushMass m of Capital c -> c)) ms
            mapM_ (\m -> let mid = fromIntegral (mushId m) :: Int in printf "ID: %d Mass: %.2f\n" mid (case mushMass m of Capital c -> c)) sortedMs

-- ==========================================
-- 3. RUNNERS
-- ==========================================

printProgressBar :: Int -> Int -> IO ()
printProgressBar current total = do
    let width = 50
    let percent = if total == 0 then 0 else fromIntegral current / fromIntegral total :: Double
    let filled = round (percent * fromIntegral width)
    let bar = replicate filled '=' ++ replicate (width - filled) '-'
    printf "\r[%s] %.1f%% (%d/%d)" bar (percent * 100) current total
    hFlush stdout

-- A. Standard Simulation
runSimulation :: String -> [Price] -> IO ()
runSimulation name prices = do
    putStrLn $ "\nInitializing " ++ name ++ "..."
    let simulation = mapM_ stepSimulation prices
    let finalState = execState simulation genesisState
    printReport "FINAL" finalState
    printMushroomDetails finalState

-- B. CSV Export (Includes Agents AND Mushrooms)
runAndExportCSV :: String -> [Price] -> IO ()
runAndExportCSV name prices = do
    hSetBuffering stdout NoBuffering
    putStrLn $ "Initializing " ++ name ++ "..."
    
    let totalTicks = length prices
    let header = "Tick,Type,ID,ParentID,X,Y,Value,Age,FractalDim\n"
    
    -- Helper to format doubles for CSV
    let fmtD = BF.formatRealFloat BF.Fixed (Just 4)

    let stateToCSVBuilder tick state =
            let
                -- 1. Agent Rows
                agentRows = mconcat $ map (\a -> 
                    let (HyphalId hid) = hypId a
                        (MushroomId pid) = hypParentId a
                        loc = hypLocation a
                        x = if not (null loc) then head loc else 0
                        y = if length loc > 1 then loc !! 1 else 0
                        (Capital bank) = bioBank (hypBiology a)
                        age = bioAge (hypBiology a)
                        d = calculateFractalDim (hypPath a)
                    in mconcat 
                        [ BI.decimal tick, B.singleton ','
                        , B.fromString "Agent", B.singleton ','
                        , BI.decimal hid, B.singleton ','
                        , BI.decimal pid, B.singleton ','
                        , fmtD x, B.singleton ','
                        , fmtD y, B.singleton ','
                        , fmtD bank, B.singleton ','
                        , BI.decimal age, B.singleton ','
                        , fmtD d, B.singleton '\n'
                        ]
                    ) (sysHyphae state)
                
                -- 2. Mushroom Rows (FIXED)
                mushRows = mconcat $ map (\m -> 
                    let (MushroomId mid) = mushId m
                        loc = mushLocation m
                        x = if not (null loc) then head loc else 0
                        y = if length loc > 1 then loc !! 1 else 0
                        (Capital mass) = mushMass m
                    in mconcat 
                        [ BI.decimal tick, B.singleton ','
                        , B.fromString "Mushroom", B.singleton ','
                        , BI.decimal mid, B.singleton ','
                        , B.singleton '0', B.singleton ',' -- ParentID (0 for root)
                        , fmtD x, B.singleton ','
                        , fmtD y, B.singleton ','
                        , fmtD mass, B.singleton ','
                        , B.singleton '0', B.singleton ',' -- Age (Mushrooms don't track age)
                        , B.fromString "0.0", B.singleton '\n' -- FractalDim (N/A)
                        ]
                    ) (sysMushrooms state)

                -- 3. Market Price Row
                (Price p) = mktPrice (sysEnv state)
                priceRow = mconcat
                    [ BI.decimal tick, B.singleton ','
                    , B.fromString "Market", B.singleton ','
                    , B.singleton '0', B.singleton ','
                    , B.singleton '0', B.singleton ','
                    , B.singleton '0', B.singleton ','
                    , B.singleton '0', B.singleton ','
                    , fmtD p, B.singleton ','
                    , B.singleton '0', B.singleton ','
                    , B.fromString "0.0", B.singleton '\n'
                    ]
            in priceRow <> mushRows <> agentRows

    let runStep (accBuilder, currentState) (tick, price) = do
            let nextState = execState (stepSimulation price) currentState
            let currentRows = stateToCSVBuilder tick nextState
            when (tick `mod` 100 == 0 || tick == totalTicks) $ 
                printProgressBar tick totalTicks
            return (accBuilder <> currentRows, nextState)

    putStrLn "Running Simulation..."
    (finalBuilder, _) <- foldM runStep (B.fromString header, genesisState) (zip [1..] prices)
    putStrLn "\nWriting CSV..."
    TIO.writeFile "mycelial_data_full.csv" (B.toLazyText finalBuilder)
    putStrLn "Done."

-- C. Backtest with Metrics
runBacktestWithMetrics :: String -> [Price] -> IO ()
runBacktestWithMetrics name prices = do
    putStrLn $ "\nRunning Backtest Analysis: " ++ name ++ "..."
    let totalTicks = length prices
    
    if totalTicks == 0 
        then putStrLn "Error: No price data available."
        else do
            -- Step Logic Accumulator
            let runStep (accTVL, currentState) (tick, price) = do
                    let nextState = execState (stepSimulation price) currentState
                    let tvl = calculateTVL nextState
                    
                    -- Strictly force 'tvl' to ensure computation happens HERE
                    let !forcedTvl = tvl 

                    -- Reduced visual updates for speed
                    when (tick `mod` 250 == 0 || tick == totalTicks) $ 
                        printProgressBar tick totalTicks
                    
                    return (forcedTvl : accTVL, nextState)

            putStrLn $ "Simulating " ++ show totalTicks ++ " trading days..."
            
            -- Run Fold
            (reversedEquity, finalState) <- foldM runStep ([], genesisState) (zip [1..] prices)
            
            let equityCurve = reverse reversedEquity

            putStrLn "\nSimulation Complete. Calculating Metrics..."
            
            -- Calculate Metrics
            let stats = calculateStats equityCurve
            
            -- Report
            printPerformanceReport stats
            printReport "FINAL STATE" finalState

-- ==========================================
-- 4. UTILS & MAIN
-- ==========================================

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

setupRun :: String -> (Int -> [Price]) -> Int -> IO ()
setupRun name generator mode = do
    durStr <- prompt "Enter Duration (ticks, default 500): "
    let duration = case readMaybe durStr of
            Just n  -> n
            Nothing -> 500
            
    case mode of
        0 -> runSimulation name (generator duration)         -- Standard
        1 -> runAndExportCSV name (generator duration)       -- CSV
        2 -> runBacktestWithMetrics name (generator duration) -- Metrics
    
    putStrLn "\nDone."
    main

-- Special Runner for S&P 500 which is IO based
runSP500 :: Int -> IO ()
runSP500 mode = do
    putStrLn "Loading Real Data from 'SPX.csv'..."
    prices <- loadSP500Data
    
    if null prices
        then putStrLn "Aborting run."
        else do
            let name = "S&P 500 (2000-2025)"
            case mode of
                0 -> runSimulation name prices
                1 -> runAndExportCSV name prices
                2 -> runBacktestWithMetrics name prices
    
    putStrLn "\nDone."
    main

main :: IO ()
main = do
    putStrLn "\n=========================================="
    putStrLn "   MYCELIAL TRADING HIVE - SIMULATOR v3.7 "
    putStrLn "=========================================="
    putStrLn "1. Bull Run"
    putStrLn "2. Bear Market"
    putStrLn "3. Volatility"
    putStrLn "4. S&P 500 (Real Data 2000-2025)"
    putStrLn "5. Export Data (CSV)"
    putStrLn "q. Quit"
    
    choice <- prompt "\nSelect: "
    case choice of
        "q" -> putStrLn "Exiting."
        "1" -> setupRun "Bull Run" generateBullRun 0
        "2" -> setupRun "Bear Market" generateBearMarket 0
        "3" -> setupRun "Volatility" generateVolatility 0
        "4" -> do
             putStrLn "Mode? (1=Standard / 2=Metrics / 3=Export):"
             scen <- prompt "> "
             case scen of
                 "1" -> runSP500 0
                 "2" -> runSP500 2
                 "3" -> runSP500 1
                 _   -> main
        "5" -> do
             putStrLn "Scenario? (1/2/3):"
             scen <- prompt "> "
             case scen of
                 "1" -> setupRun "Bull Run" generateBullRun 1
                 "2" -> setupRun "Bear Market" generateBearMarket 1
                 "3" -> setupRun "Volatility" generateVolatility 1
                 _   -> main
        _   -> main