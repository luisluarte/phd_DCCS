module MycelialMetrics where

import Data.List (foldl', minimumBy, maximumBy)
import Data.Ord (comparing)
import Text.Printf (printf)

-- ==========================================
-- DATA TYPES
-- ==========================================

data PerformanceStats = PerformanceStats
  { totalReturnRaw     :: Double
  , totalReturnPercent :: Double
  , maxDrawdown        :: Double
  , sharpeRatio        :: Double
  , sortinoRatio       :: Double
  , volatility         :: Double -- Annualized Volatility
  , winRate            :: Double
  , bestTick           :: Double
  , worstTick          :: Double
  } deriving (Show)

-- ==========================================
-- MATH HELPERS
-- ==========================================

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

stdDev :: [Double] -> Double
stdDev xs =
  let avg = mean xs
      sqDiffs = map (\x -> (x - avg) ** 2) xs
  in sqrt (sum sqDiffs / fromIntegral (length xs - 1))

-- Downside Deviation for Sortino (only penalizes negative returns)
downsideDev :: [Double] -> Double
downsideDev xs =
  let avg = mean xs 
      negatives = map (\x -> min 0.0 x) xs 
      sqDiffs = map (** 2) negatives
  in sqrt (sum sqDiffs / fromIntegral (length xs - 1))

-- ==========================================
-- CORE CALCULATIONS
-- ==========================================

calculateStats :: [Double] -> PerformanceStats
calculateStats [] = error "Cannot calculate stats on empty history"
calculateStats equityCurve =
  let
    initialCapital = head equityCurve
    finalCapital = last equityCurve
    
    -- 1. PnL
    pnlRaw = finalCapital - initialCapital
    pnlPct = (finalCapital - initialCapital) / initialCapital

    -- 2. Returns (Arithmetic)
    -- r_t = (P_t - P_{t-1}) / P_{t-1}
    pairs = zip equityCurve (tail equityCurve)
    returns = map (\(prev, curr) -> (curr - prev) / prev) pairs

    -- 3. Volatility & Ratios
    -- We assume ~252 trading days for annualization (Stocks)
    avgReturn = mean returns
    dailyVol = stdDev returns
    annualVol = dailyVol * sqrt 252.0
    
    downside = downsideDev returns
    annualDownside = downside * sqrt 252.0

    -- Annualized Returns approx
    annualizedReturn = avgReturn * 252.0

    -- Sharpe (Assuming Risk Free Rate = 0 for simplicity)
    sharpe = if annualVol == 0 then 0 else annualizedReturn / annualVol
    
    -- Sortino
    sortino = if annualDownside == 0 then 0 else annualizedReturn / annualDownside

    -- 4. Max Drawdown
    -- Calculate running maximum, then find max deviation from that running max
    runningMax = scanl1 max equityCurve
    drawdowns = zipWith (\curr peak -> (curr - peak) / peak) equityCurve runningMax
    mdd = minimum drawdowns -- Drawdowns are negative numbers

    -- 5. Win Rate
    positiveTicks = length $ filter (> 0) returns
    totalTicks = length returns
    winR = fromIntegral positiveTicks / fromIntegral totalTicks

  in PerformanceStats
    { totalReturnRaw = pnlRaw
    , totalReturnPercent = pnlPct * 100.0
    , maxDrawdown = mdd * 100.0
    , sharpeRatio = sharpe
    , sortinoRatio = sortino
    , volatility = annualVol * 100.0
    , winRate = winR * 100.0
    , bestTick = maximum returns * 100.0
    , worstTick = minimum returns * 100.0
    }

-- ==========================================
-- PRETTY PRINTER
-- ==========================================

printPerformanceReport :: PerformanceStats -> IO ()
printPerformanceReport stats = do
    putStrLn "\n=============================================="
    putStrLn "   PERFORMANCE METRICS (PORTFOLIO LEVEL)      "
    putStrLn "=============================================="
    printf "Total PnL (Raw):      %10.2f\n" (totalReturnRaw stats)
    printf "Total Return:         %10.2f%%\n" (totalReturnPercent stats)
    putStrLn "----------------------------------------------"
    printf "Max Drawdown:         %10.2f%%\n" (maxDrawdown stats)
    printf "Volatility (Ann.):    %10.2f%%\n" (volatility stats)
    putStrLn "----------------------------------------------"
    printf "Sharpe Ratio:         %10.4f\n" (sharpeRatio stats)
    printf "Sortino Ratio:        %10.4f\n" (sortinoRatio stats)
    putStrLn "----------------------------------------------"
    printf "Win Rate:             %10.2f%%\n" (winRate stats)
    printf "Best Tick:            %10.2f%%\n" (bestTick stats)
    printf "Worst Tick:           %10.2f%%\n" (worstTick stats)
    putStrLn "==============================================\n"