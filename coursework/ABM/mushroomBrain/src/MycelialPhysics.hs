module MycelialPhysics where

import MycelialState
import Data.List (foldl')

-- | L2 Norm for the parameter space
euclideanDistance :: ParamVector -> ParamVector -> Double
euclideanDistance v1 v2 = sqrt $ sum $ zipWith (\x1 x2 -> (x1 - x2)^(2 :: Int)) v1 v2

-- | Clamping logic for spatial boundaries
clampVector :: ParamVector -> ParamVector
clampVector = map (max 0.0 . min 1.0)

-- | Calculation of agent "Mass" or "Pressure" for physics interactions
calculateHyphalPressure :: HyphalTip -> Double
calculateHyphalPressure agent = 
    let pos = hypHoldings agent
        avg = hypAvgEntry agent
        -- Use the helper from MycelialState
        Capital c = calculatePosCost pos avg
        Capital b = bioBank (hypBiology agent)
    in c + b

-- | Fractal dimension calculation for the mycelial network
calculateFractalDim :: [ParamVector] -> Double
calculateFractalDim points = 
    if length points < 2 then 0.0 
    else 
        let d = sum [euclideanDistance p1 p2 | (p1, p2) <- zip points (tail points)]
        in log d / log (fromIntegral (length points))

-- | Age-based decay or growth scaling
calculateAgeFactor :: BioState -> Double
calculateAgeFactor bio = 
    let tau = fromIntegral (bioAge bio) :: Double -- Uses restored bioAge
    in exp (-0.01 * tau)
