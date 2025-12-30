module MycelialPhysics where

import MycelialState

-- | 1. EXTRACT VECTOR
-- Gets the shape [Current, Lag1, Lag2] at a specific time 't'
getPhaseVector :: [Double] -> Int -> Int -> Int -> Maybe [Double]
getPhaseVector history t lag1 lag2
    | t < max lag1 lag2 = Nothing -- Not enough history
    | otherwise = 
        let p0 = history !! t
            p1 = history !! (t - lag1)
            p2 = history !! (t - lag2)
        in Just [p0, p1, p2]

-- | 2. EUCLIDEAN DISTANCE
dist :: [Double] -> [Double] -> Double
dist v1 v2 = sqrt $ sum $ zipWith (\a b -> (a - b)^2) v1 v2

-- | 3. NUTRIENT KERNEL (The "Digestion")
-- Scans history to find how predictive the lags (l1, l2) are.
calculateNutrient :: [Double] -> Int -> Int -> Int -> Double -> Double
calculateNutrient history tNow l1 l2 sigma =
    case getPhaseVector history tNow l1 l2 of
        Nothing -> 0.0
        Just currentVec -> 
            let 
                -- We scan historical points (from t=maxLag to tNow-1)
                -- In a real GPU impl, this is parallel. Here we sample specific points or full scan.
                -- For speed, let's scan the last 100 ticks.
                lookbackWindow = 100
                startIndex = max (max l1 l2 + 1) (tNow - lookbackWindow)
                scanIndices = [startIndex .. (tNow - 1)]
                
                -- Function to test a historical moment 'k'
                testHistory k = 
                    case getPhaseVector history k l1 l2 of
                        Nothing -> 0.0
                        Just pastVec ->
                            let d = dist currentVec pastVec
                                -- Similarity (Gaussian Kernel)
                                similarity = exp ( - (d * d) / (2 * sigma * sigma))
                            in if similarity > 0.1 -- Only check prediction if shapes match
                               then 
                                   -- DID IT PREDICT THE FUTURE?
                                   -- Compare Current Step (tNow -> ??) vs Past Step (k -> k+1)
                                   -- Note: We assume we know P_tNow. Real prediction checks P_tNow+1 vs P_k+1
                                   -- Here we measure "Coherence": Does the shape imply the same value?
                                   similarity
                               else 0.0
                
                totalResonance = sum (map testHistory scanIndices)
                
                -- Normalize roughly
                normalized = totalResonance / fromIntegral (length scanIndices + 1)
            in normalized * 10.0 -- Scale up for visibility
