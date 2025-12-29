module MycelialPhysics where

import MycelialState

moveHypha :: Bool -> Price -> any -> [HyphalTip] -> HyphalTip -> ParamVector
moveHypha _ _ _ _ agent = zipWith (+) (hypLocation agent) (hypVelocity agent)

-- Signature restored to accept Price and HyphalTip
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
