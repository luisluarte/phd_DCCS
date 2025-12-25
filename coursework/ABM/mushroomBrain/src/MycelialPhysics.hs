module MycelialPhysics where

import MycelialState


-- ====================================
-- METRIC SPACE DEFINITION
-- ====================================

euclideanDistance :: ParamVector -> ParamVector -> Double
euclideanDistance v1 v2 =
  sqrt . sum $ zipWith (\x y -> (x - y)^2) v1 v2

-- ====================================
-- PARAMETER SPACE BOUNDARIES
-- ====================================
getBounds :: Int -> (Double, Double)
getBounds 0 = (0.001, 0.99) -- drop threshold
getBounds 1 = (0.001, 1000.0) -- profit target
getBounds _ = (-1000.0, 1000.0) -- fallback

clampVector :: ParamVector -> ParamVector
clampVector vec =
  let 
    reflect val (minVal, maxVal)
      | val < minVal = minVal + (minVal - val) -- bounce off min
      | val > maxVal = maxVal - (val - maxVal) -- bounce off max
      | otherwise = val

    safety val (minVal, maxVal) = max minVal (min maxVal val)

    -- Fixed: Pattern match the tuple (val, idx)
    process (val, idx) =
      let bounds = getBounds idx
      in safety (reflect val bounds) bounds

    indexed = zip vec [0..]
  in
    map process indexed

-- ====================================
-- FRACTAL DIMENSION (D)
-- ====================================

-- compute fractal dimension of the agent's path
-- D = log(Total Path Length) / log(Net Displacement)
calculateFractalDim :: [ParamVector] -> Double
calculateFractalDim p | length p < 5 = 1.0 -- Need history to establish trend
calculateFractalDim path =
  let
    currentPos = head path 
    startPos = last path 

    netDisp = euclideanDistance startPos currentPos 
    steps = zip path (tail path)
    
    -- Calculate stats
    n = fromIntegral (length steps) :: Double
    totalPathLen = sum [euclideanDistance p1 p2 | (p1, p2) <- steps]
    avgStep = if n > 0 then totalPathLen / n else 0.0
    
    -- Avoid singularities
    safeDisp = max 1.0e-9 netDisp
    safeStep = max 1.0e-9 avgStep
    ratio = safeDisp / safeStep

  in
    if ratio <= 1.0 
       -- If displacement is less than a single step (looped back), D is maxed
       then 2.0 
       -- Standard Fractal Dimension for random walks
       else 
          let d = log n / log ratio
          in max 1.0 (min 2.0 d) -- Clamp to valid range [1, 2]

-- ====================================
-- FLOW RATE (Q_f)
-- ====================================

-- computes the mycelial network flow rate based on its topological complexity
-- Q = 1 - (1 / 2^D)
-- range: [0.5, 1.0]
calculateFlowRate :: Double -> Double
calculateFlowRate d = 1.0 - (1.0 / (2.0 ** d))

-- ====================================
-- HYDRAULIC PRESSURE (Psi)
-- ====================================

-- compute the internal hydraulic pressure of an agent
-- Formula: Psi = beta * (TotalEquity * Q_f) - (1 - beta) * (Stress / Age)
calculatePressure :: Price -> HyphalTip -> Double
calculatePressure (Price currentPrice) agent =
  let
    -- unpack data
    genome = hypGenome agent
    bio = hypBiology agent
    pos = hypHoldings agent

    -- compute pnl
    Quantity q = posQuantity pos
    Capital c = posCost pos

    currentValue = q * currentPrice
    unrealized = currentValue - c -- with this we can derive stress

    -- get agent variables
    Capital piBank = bioBank bio
    
    -- FIXED: Use Total Wealth (Cash + Asset Value) for pressure source
    -- This prevents pressure from dropping to zero when agents go "All In"
    totalWealth = piBank + currentValue

    -- convert Age (Int) to Double for math
    tau = fromIntegral (bioAge bio) :: Double

    -- compute D and Q_f from history
    d = calculateFractalDim (hypPath agent)
    q_f = calculateFlowRate d

    -- get parameters
    beta1 = geneGreed genome
    epsilon = 1.0 -- small constant to prevent division by zero if Age = 0

    -- pressure equation
    sourceTerm = beta1 * (totalWealth * q_f)

    -- resistance: unrealized loss dampened by Age
    -- FIXED: Only count negative PnL as stress. 
    loss = if unrealized < 0 then abs unrealized else 0.0

    stressTerm = (1.0 - beta1) * (loss / (tau + epsilon))

    in
      sourceTerm - stressTerm