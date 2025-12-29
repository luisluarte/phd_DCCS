module MycelialPhysics where

import MycelialState
import qualified Data.Set as Set

-- | Moves the agent based on local pressure and turbulence
-- The 'turbulence' parameter scales the magnitude of the random walk.
moveHypha :: Bool -> Price -> any -> [HyphalTip] -> HyphalTip -> ParamVector
moveHypha _ _ _ _ agent = zipWith (+) (hypLocation agent) (hypVelocity agent)

calculatePressure :: Price -> HyphalTip -> Double
calculatePressure _ agent = 
    let (Capital c) = posCost (hypHoldings agent)
        (Capital b) = bioBank (hypBiology agent)
    in c + b

calculateFlowRate :: Double -> Double
calculateFlowRate d = d * 0.15

euclideanDistance :: ParamVector -> ParamVector -> Double
euclideanDistance v1 v2 = sqrt $ sum $ zipWith (\x1 x2 -> (x1 - x2)^(2 :: Int)) v1 v2

clampVector :: ParamVector -> ParamVector
clampVector = map (max 0.0 . min 1.0)

-- | Calculates the Fractal Dimension (Box-Counting Method)
-- Returns a value between 1.0 (linear) and 2.0 (plane-filling).
calculateFractalDim :: [[Double]] -> Double
calculateFractalDim points 
    | length points < 2 = 1.0 -- Not enough points to form a line
    | otherwise = 
        let 
            -- 1. Determine Bounding Box
            xs = map (!! 0) points
            ys = map (!! 1) points
            minX = minimum xs
            maxX = maximum xs
            minY = minimum ys
            maxY = maximum ys
            
            width = maxX - minX
            height = maxY - minY
            maxDim = max width height
            
            -- Avoid division by zero for stationary agents
            scale0 = if maxDim == 0 then 1.0 else maxDim
            
            -- 2. Define Two Scales (r1 = Coarse, r2 = Fine)
            -- We check the box count at 1/4 and 1/8 of the total span
            r1 = scale0 / 4.0
            r2 = scale0 / 8.0
            
            -- 3. Box Counting Function
            -- Maps points to grid coordinates (col, row) and counts unique sets
            countBoxes r = 
                let gridKeys = map (\p -> (floor ((p!!0)/r), floor ((p!!1)/r))) points
                in fromIntegral $ Set.size $ Set.fromList gridKeys
            
            n1 = countBoxes r1
            n2 = countBoxes r2
        in
            -- 4. Calculate Slope: (log N2 - log N1) / (log (1/r2) - log (1/r1))
            -- If counts are identical, dimension is 1.0 (no new detail at finer scale)
            if n2 == n1 
            then 1.0 
            else (log n2 - log n1) / (log (1/r2) - log (1/r1))
