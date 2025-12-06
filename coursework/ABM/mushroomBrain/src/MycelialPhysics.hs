module MycelialPhysics where

import MycelialState


-- ====================================
-- METRIC SPACE DEFINITION
-- ====================================

euclideanDistance :: ParamVector -> ParamVector -> Double
euclideanDistance v1 v2 =
  sqrt . sum $ zipWith (\x y -> (x - y)^2) v1 v2

-- ====================================
-- FRACTAL DIMENSION (D)
-- ====================================

-- compute fractal dimension of the agent's path
-- D = log(Total Path Length) / log(Net Displacement)
calculateFractalDim :: [ParamVector] -> Double
calculateFractalDim [] = 1.0 -- if history is empty return 1.0
calculateFractalDim [_] = 1.0 -- if history is of length 1, return 1.0
calculateFractalDim path =
  let
    currentPos = head path -- do not forget! newest element is at the head
    startPos = last path -- last is the start of the path

    netDisp = euclideanDistance startPos currentPos -- distance start -> end

    steps = zip path (tail path) -- this makes pairs (step1, step2) (step2, step3) ...
    pathLen = sum [euclideanDistance p1 p2 | (p1, p2) <- steps]

  in
    if netDisp < 1.0e-9 -- to deal with super small hyphae
      then 2.0 -- this is just to avoid division by zero
      else max 1.0 (log pathLen / log netDisp)
      -- avoid fractal dimension being < 1.0

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
-- Formula: Psi = beta * (profit * Q_f) - (1 - beta) * (Stress / Age)
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

    -- convert Age (Int) to Double for math
    tau = fromIntegral (bioAge bio) :: Double

    -- compute D and Q_f from history
    d = calculateFractalDim (hypPath agent)
    q_f = calculateFlowRate d

    -- get parameters
    beta1 = geneGreed genome
    epsilon = 1.0 -- small constant to prevent division by zero if Age = 0

    -- pressure equation
    sourceTerm = beta1 * (piBank * q_f)

    -- resistance: unrealized loss dampened by Age
    -- we take abosolute value of loss because pressure equation substracts it
    stressTerm = (1.0 - beta1) * (abs unrealized / (tau + epsilon))

    in
      sourceTerm - stressTerm
