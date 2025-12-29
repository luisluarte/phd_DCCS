module Simulation.Micro 
    ( executeTrade
    , executeSell
    , moveAgent
    , processMicroFlow -- Keeping original export if needed
    , spawnHypha       -- Keeping original export if needed
    ) where

import MycelialState
import MycelialStrategy
import MycelialPhysics (calculateFlowRate, clampVector)
import qualified Simulation.Types as T
import Simulation.Accessors hiding (Sim)
import System.Random (StdGen, randomR)

-- ========================================================
-- MOVEMENT LOGIC
-- ========================================================

-- | Moves the agent based on local pressure and random brownian motion
moveAgent :: Double -> HyphalTip -> StdGen -> HyphalTip
moveAgent pressure agent rng =
    let
        loc = hypLocation agent
        -- Simple Brownian motion scale
        scale = 0.01 
        
        -- Generate random deltas
        (dx, rng1) = randomR (-scale, scale) rng
        (dy, _)    = randomR (-scale, scale) rng1
        
        -- Apply velocity or pressure influence if desired
        -- For now, just adding random noise to current location
        newLocRaw = zipWith (+) loc [dx, dy]
        newLoc = clampVector newLocRaw -- Ensure it stays in [0,1] bounds
    in
        agent 
          { hypLocation = newLoc
          , hypPath = newLoc : hypPath agent 
          -- Note: You might want to update hypVelocity here too if using momentum
          }

-- ========================================================
-- TRADING LOGIC
-- ========================================================

-- | Executes a BUY order (Entry)
-- Returns: Just (UpdatedAgent, Cost) if successful
executeTrade :: Price -> HyphalTip -> Maybe (HyphalTip, Capital)
executeTrade (Price p) agent =
    let 
        (Capital bank) = bioBank (hypBiology agent)
        -- Fixed order size (e.g., 1.0 unit of currency) or percentage?
        -- Using simple fixed logic from previous iterations:
        orderCostVal = 1.0 
    in
        if bank > orderCostVal
        then 
            let
                pos = hypHoldings agent
                (Quantity currentQ) = posQuantity pos
                (Capital currentCost) = posCost pos
                
                -- Execute Buy
                quantityBought = orderCostVal / p
                newPos = Position (Quantity (currentQ + quantityBought)) (Capital (currentCost + orderCostVal))
                
                newBank = Capital (bank - orderCostVal)
                newStepCount = hypStepCount agent + 1
                
                newAgent = agent
                    { hypHoldings = newPos
                    , hypBiology = (hypBiology agent) { bioBank = newBank }
                    , hypStepCount = newStepCount
                    -- Should we update RefPrice on buy? usually on Sell or first Buy.
                    -- If step count was 0, maybe set ref price?
                    , hypRefPrice = if hypStepCount agent == 0 then Price p else hypRefPrice agent
                    }
            in
                Just (newAgent, Capital orderCostVal)
        else
            Nothing

-- | Executes a SELL order (Exit)
-- Returns: Just (UpdatedAgent, Revenue, Profit)
executeSell :: Price -> HyphalTip -> Maybe (HyphalTip, Capital, Capital)
executeSell (Price p) agent =
    let
        pos = hypHoldings agent 
        (Quantity q) = posQuantity pos
        (Capital cost) = posCost pos
    in
        if q <= 1e-9 then Nothing
        else
            let
                -- 1. Calculate Revenue
                revenueVal = q * p
                revenue = Capital revenueVal
                
                -- 2. Calculate Profit (Surplus)
                profitVal = revenueVal - cost
                profit = Capital profitVal
                
                (Capital currentBank) = bioBank (hypBiology agent)
                
                -- 3. Update Bank
                newBank = Capital (currentBank + revenueVal) 
                
                newAgent = agent
                    { hypHoldings = mempty -- Clear inventory
                    , hypRefPrice = Price p  -- Reset reference for next cycle
                    , hypStepCount = 0       -- Reset DCA counter
                    , hypBiology = (hypBiology agent) { bioBank = newBank }
                    }
            in
                Just (newAgent, revenue, profit)

-- ========================================================
-- LEGACY / HELPER FUNCTIONS (Kept to avoid breaking other imports)
-- ========================================================

processMicroFlow :: HyphalTip -> Price -> T.Sim HyphalTip
processMicroFlow agent (Price p) = do
    let (Capital b) = bioBank (hypBiology agent)
        flow = calculateFlowRate (fromIntegral (bioAge (hypBiology agent)))
    return agent { hypBiology = (hypBiology agent) { bioBank = Capital (b + flow) } }

spawnHypha :: MushroomId -> HyphalId -> [Double] -> Genome -> Price -> HyphalTip
spawnHypha mid hid loc genes p =
    HyphalTip hid mid loc [0,0] [loc] (Position 0 0) (BioState 0 (Capital 10)) genes p 0
