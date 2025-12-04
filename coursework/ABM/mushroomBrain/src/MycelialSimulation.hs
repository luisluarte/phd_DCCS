module MycelialSimulation where

import MycelialState
import MycelialPhysics
import System.Random (StdGen, mkStdGen, randomR)
import Control.Monad (replicateM_, when, unless)
import Control.Monad.State (State, get, put, gets, modify, runState, execState)
import Data.Maybe (mapMaybe, catMaybes)


-- ======================
-- simulation monad
-- ======================

type Sim a = State SystemState a

-- get simulation tick/time
getTime :: Sim Time
getTime = gets sysTime

-- get current price from environment
getPrice :: Sim Price
getPrice = gets (mktPrice . sysEnv)

-- get global wallet
getWallet :: Sim GlobalWallet
getWallet = gets sysWallet

-- modify wallet
modifyWallet :: (Capital -> Capital) -> Sim ()
modifyWallet f = modify $ \s ->
    let (GlobalWallet c) = sysWallet s
    in s { sysWallet = GlobalWallet (f c) }

-- get all active hyphal tips
getAgents :: Sim [HyphalTip]
getAgents = gets sysHyphae


-- overwrite the list of agents
setAgents :: [HyphalTip] -> Sim ()
setAgents newAgents = modify $ \s -> s { sysHyphae = newAgents }

getMushrooms :: Sim [MushroomBody]
getMushrooms = gets sysMushrooms

setMushrooms :: [MushroomBody] -> Sim ()
setMushrooms newMushrooms = modify $ \s -> s { sysMushrooms = newMushrooms }

-- ======================
-- MACRO PARAMETERS
-- ======================

-- radius of influence for a single agent
sensingRadius :: Double
sensingRadius = 0.05

-- minimum pressure required to spawn mushroom
mushroomThreshold :: Double
mushroomThreshold = 500.0

-- how much capital a mushroom drains per tick per unit of traffic
drainRate :: Double
drainRate = 0.1

-- Vacuum coefficient for drain equation
vacuumCoefficient :: Double
vacuumCoefficient = 0.1

-- ======================
-- ACTION LOGIC (micro)
-- ======================

-- execute buy: calculate Q_f, deducts wallet, updates position
executeTrade :: Price -> HyphalTip -> Capital -> Maybe (HyphalTip, Capital)
executeTrade (Price p) agent (Capital walletBalance) =
    let
        genes = hypGenome agent
        step = hypStepCount agent

        -- check max orders
        maxOrders = geneMaxOrders genes

    in
        if step >= maxOrders
            then Nothing -- max depth reached, cannot buy
            else
                let
                    -- calculate multipliers
                    volMult = if step == 0 then 1.0 else (geneVolMult genes) ^ step

                    -- physics modulation (fractal flow)
                    d = calculateFractalDim (hypPath agent)
                    q_f = calculateFlowRate d

                    -- calculate order size
                    baseAmt = if step == 0 then geneBaseOrder genes else geneDCAOrder genes

                    -- calculate cost
                    orderCostVal = baseAmt * volMult * q_f

                    -- check wallet
                    isAffordable = orderCostVal <= walletBalance
                in
                    if not isAffordable
                        then Nothing -- no money for trade
                        else
                            let
                                orderCost = Capital orderCostVal
                                orderQty = Quantity (orderCostVal / p)
                                newPos = (hypHoldings agent) <> Position orderQty orderCost

                                newAgent = agent
                                    {
                                    hypHoldings = newPos,
                                    hypRefPrice = Price p,
                                    hypStepCount = step + 1 -- increment step
                                    }

                            in
                                Just (newAgent, orderCost)

-- move: updates location (laminar vs turbulent)
moveAgent :: Double -> HyphalTip -> StdGen -> HyphalTip
moveAgent pressure agent rng =
    let
        turbulenceThreshold = 10.0 -- defined locally or globally
        -- entropy Sigmoid
        k = 1.0
        entropy = 1.0 / (1.0 + exp (-(k * (pressure - turbulenceThreshold))))
        currentLoc = hypLocation agent
        currentVel = hypVelocity agent
        (r1, rng1) = randomR (-1.0, 1.0) rng
        (r2, _) = randomR (-1.0, 1.0) rng1
        randomVec = [r1, r2]
        newVel = zipWith (\v r -> (1.0 - entropy) * v + entropy * r) currentVel randomVec

        -- growth rate from genome
        eta = geneGrowthRate (hypGenome agent)
        newLoc = zipWith (\x v -> x + eta * v) currentLoc newVel
    in
        agent { hypLocation = newLoc, hypVelocity = newVel, hypPath = newLoc : hypPath agent }



-- ======================
-- MACRO DYNAMICS (Ecosystem)
-- ======================

-- compute pheromone field intensity at location x
calculateLocalField :: ParamVector -> [HyphalTip] -> Price -> Double
calculateLocalField loc agents currentPrice =
  let
    kernel r = exp (-(r**2) / (2 * (sensingRadius/3)**2))
    contributions = map (\a -> 
      let 
        dist = euclideanDistance loc (hypLocation a)
        pressure = calculatePressure currentPrice a
      in if dist < sensingRadius
        then pressure * kernel dist
        else 0
        ) agents
  in
    sum contributions

-- check and spawn mushrooms
spawnMushrooms :: Price -> Sim ()
spawnMushrooms currentPrice = do
    agents <- getAgents
    mushrooms <- getMushrooms

    -- candidate locations are current agent positions
    let candidates = [hypLocation a | a <- agents]

    -- filter candidates that exceed threshold
    let newSpawns = filter (\loc -> calculateLocalField loc agents currentPrice > mushroomThreshold) candidates

    -- filter out candidates too close to existing mushrooms
    let validSpawns = filter (\loc -> all (\m -> euclideanDistance loc (mushLocation m) > sensingRadius) mushrooms) newSpawns

    -- create mushroom
    let newMushroomBody = case validSpawns of
          [] -> []
          (loc:_) -> [MushroomBody
           {
           mushId = length mushrooms + 1,
           mushLocation = loc,
           mushMass = Capital 0.0,
           mushGenome = Genome 0.5 10.0 0.001 1000.0 0.1 10.0 20.0 5 1.1 1.2
           }]

    unless (null newMushroomBody) $ do
      setMushrooms (mushrooms ++ newMushroomBody)

-- Apply Sink Phase (Drain Capital)
applyDrain :: HyphalTip -> [MushroomBody] -> Price -> [HyphalTip] -> (HyphalTip, Capital)
applyDrain agent mushrooms currentPrice allAgents =
    let
        dists = map (\m -> (m, euclideanDistance (hypLocation agent) (mushLocation m))) mushrooms
        nearby = filter (\(_, d) -> d < sensingRadius) dists
    in
        case nearby of
            [] -> (agent, Capital 0.0)
            ((m, _):_) -> 
                let
                    psi_i = calculatePressure currentPrice agent
                    phi_m = calculateLocalField (mushLocation m) allAgents currentPrice
                    
                    psi_m = -vacuumCoefficient * phi_m
                    
                    tau = fromIntegral (bioAge (hypBiology agent)) :: Double
                    
                    fluxVal = tau * (psi_i + (vacuumCoefficient * phi_m))
                    
                    drainAmount = Capital (max 0.0 fluxVal)
                    
                    (Capital currentBank) = bioBank (hypBiology agent)
                    newBank = Capital (currentBank - max 0.0 fluxVal)
                    newBio = (hypBiology agent) { bioBank = newBank }
                in
                    (agent { hypBiology = newBio }, drainAmount)


-- ======================
-- UPDATE LOOP
-- ======================

updateHypha :: Price -> [MushroomBody] -> [HyphalTip] -> HyphalTip -> Sim (Maybe HyphalTip)
updateHypha currentPrice mushrooms allAgents agent = do
    -- A. Apply Sink Phase
    let (agentAfterTax, drainedAmt) = applyDrain agent mushrooms currentPrice allAgents
    
    -- B. Calculate Pressure
    let psi = calculatePressure currentPrice agentAfterTax
    let dieThresh = -50.0
    let (Capital bank) = bioBank (hypBiology agentAfterTax)

    if psi < dieThresh || bank < 0
        then return Nothing
        else do
            let (Price refP) = hypRefPrice agent
            let (Price currP) = currentPrice

            -- get deviation from location vector [delta, tau]
            let dev = case hypLocation agent of
                        (d:_) -> d
                        []    -> 0.01
            
            let genes = hypGenome agent
            let step = hypStepCount agent

            let devMult = if step == 0 then 1.0 else (geneDevMult genes) ^ step
            let effectiveDev = dev * devMult

            let shouldBuy = currP <= refP * (1.0 - effectiveDev)

            -- attempt trade
            agentAfterTrade <- if shouldBuy
                then do
                    (GlobalWallet balance) <- getWallet
                    case executeTrade currentPrice agentAfterTax balance of
                        Just (newAg, cost) -> do
                            modifyWallet (\c -> c - cost)
                            return newAg
                        Nothing -> return agentAfterTax 
                else return agentAfterTax

            t <- getTime
            let (Time tick) = t
            let rng = mkStdGen (hypId agent + tick * 1000)
            let agentAfterMove = moveAgent psi agentAfterTrade rng

            return (Just agentAfterMove)


stepSimulation :: Price -> Sim ()
stepSimulation newPrice = do
    agents <- getAgents
    mushrooms <- getMushrooms
    
    -- Note: Passing 'agents' as 'allAgents' means using state from start of tick
    maybeAgents <- mapM (updateHypha newPrice mushrooms agents) agents
    let survivingAgents = catMaybes maybeAgents
    setAgents survivingAgents
    
    spawnMushrooms newPrice
    modify $ \s -> s { sysTime = sysTime s + 1 }

-- ======================
-- TEST
-- ======================

genesisState :: SystemState
genesisState = SystemState
    { sysTime      = Time 0
    , sysWallet    = GlobalWallet 10000.0
    , sysEnv       = Environment (Price 100.0) []
    , sysHyphae    = [testAgent]
    , sysMushrooms = []
    , sysSpores    = []
    }
  where
    testGenome = Genome
        {
        geneGreed = 0.5,
        geneTurbulence = 10.0,
        geneGrowthRate = 0.001,
        geneMaturity = 1000.0,
        geneDispersion = 0.1,
        geneBaseOrder = 10.0,
        geneDCAOrder = 20.0,
        geneMaxOrders = 5,
        geneDevMult = 1.1,
        geneVolMult = 1.2
    }
    testPos    = Position (Quantity 1.0) (Capital 100.0)
    testBio    = BioState 10 (Capital 50.0)

    testAgent = HyphalTip
        { hypId       = 1
        , hypLocation = [0.01, 0.05]
        , hypVelocity = [0.0, 0.0]
        , hypPath     = [[0.01, 0.05]]
        , hypHoldings = testPos
        , hypBiology  = testBio
        , hypGenome   = testGenome
        , hypRefPrice = Price 100.0
        , hypStepCount = 0
        }

-- run sim for N steps
runTest :: Int -> IO ()
runTest steps = do
    putStrLn $ "--- starting simulation for " ++ show steps ++ " steps ---"

    let prices = [Price (100.0 - fromIntegral i) | i <- [1..steps]]
    let simulation = mapM_ stepSimulation prices
    let finalState = execState simulation genesisState

    putStrLn $ "final time: " ++ show (sysTime finalState)
    putStrLn $ "wallet: " ++ show (sysWallet finalState)
    putStrLn $ "hyphae count: " ++ show (length $ sysHyphae finalState)
    putStrLn $ "mushroom count: " ++ show (length $ sysMushrooms finalState)

    case sysHyphae finalState of
        [] -> putStrLn "agent died."
        (h:_) -> do
            putStrLn $ "agent step: " ++ show (hypStepCount h)
            putStrLn $ "agent holdings: " ++ show (hypHoldings h)

main :: IO ()
main = runTest 10