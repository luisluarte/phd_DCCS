module MycelialSimulation where

import MycelialState
import MycelialPhysics
import MycelialStrategy
import System.Random (StdGen, mkStdGen, randomR)
import Control.Monad (replicateM_, when, unless)
import Control.Monad.State (State, get, put, gets, modify, runState, execState)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (partition, foldl', sortBy)
import Data.Ord (comparing)

-- ... (Imports and Monad setup remain the same) ...

type Sim a = State SystemState a

getTime :: Sim Time
getTime = gets sysTime

getPrice :: Sim Price
getPrice = gets (mktPrice . sysEnv)

getWallet :: Sim GlobalWallet
getWallet = gets sysWallet

modifyWallet :: (Capital -> Capital) -> Sim ()
modifyWallet f = modify $ \s ->
    let (GlobalWallet c) = sysWallet s
    in s { sysWallet = GlobalWallet (f c) }

getAgents :: Sim [HyphalTip]
getAgents = gets sysHyphae

setAgents :: [HyphalTip] -> Sim ()
setAgents newAgents = modify $ \s -> s { sysHyphae = newAgents }

getMushrooms :: Sim [MushroomBody]
getMushrooms = gets sysMushrooms

setMushrooms :: [MushroomBody] -> Sim ()
setMushrooms newMushrooms = modify $ \s -> s { sysMushrooms = newMushrooms }

getSpores :: Sim [Spore]
getSpores = gets sysSpores

setSpores :: [Spore] -> Sim ()
setSpores newSpores = modify $ \s -> s { sysSpores = newSpores }

-- ======================
-- MACRO PARAMETERS
-- ======================

sensingRadius :: Double
sensingRadius = 0.05

dieThresh :: Double
dieThresh = -50.0

-- ======================
-- EVOLUTION
-- ======================

mutateFloat :: Double -> Double -> StdGen -> (Double, StdGen)
mutateFloat val stdDev rng =
    let (noise, newRng) = randomR (-stdDev, stdDev) rng
    in (max 0.001 (val + noise), newRng)

mutateGenome :: Genome -> StdGen -> Genome
mutateGenome g rng =
    let
        (r1, rng1) = mutateFloat (geneGreed g) 0.05 rng
        (r2, rng2) = mutateFloat (geneTurbulence g) 1.0 rng1
        (r3, rng3) = mutateFloat (geneGrowthRate g) 0.0005 rng2
        (r4, rng4) = mutateFloat (geneBaseOrder g) 2.0 rng3
        (r5, rng5) = mutateFloat (genePhiCritical g) 5.0 rng4
        (r6, rng6) = mutateFloat (geneReproductiveInvest g) 0.05 rng5
        (r7, rng7) = mutateFloat (geneVacuumCoefficient g) 0.01 rng6
        (r8, rng8) = mutateFloat (geneDevMult g) 0.05 rng7
    in
        g { geneGreed = min 0.99 r1
          , geneTurbulence = r2
          , geneGrowthRate = r3
          , geneBaseOrder = r4
          , geneBaseOrder = r5
          , geneReproductiveInvest = min 0.9 (max 0.1 r6)
          , geneVaccumCoefficient = min 1.0 (max 0.01 r7)
          , geneDevMult = r8
          }

-- ======================
-- ACTION LOGIC (Micro)
-- ======================

executeSell :: Price -> HyphalTip -> Maybe (HyphalTip, Capital, Capital)
executeSell (Price p) agent =
    let
        pos = hypHoldings agent
        (Quantity q) = posQuantity pos
        (Capital cost) = posCost pos
    in
        if q <= 0 then Nothing
        else
            let
                revenueVal = q * p
                revenue = Capital revenueVal
                profit = Capital (revenueVal - cost)
                (Capital currentBank) = bioBank (hypBiology agent)
                newBank = Capital (currentBank + (revenueVal - cost))
                newAgent = agent
                    { hypHoldings = mempty
                    , hypRefPrice = Price p
                    , hypStepCount = 0
                    , hypBiology = (hypBiology agent) { bioBank = newBank }
                    }
            in
                Just (newAgent, revenue, profit)

executeTrade :: Price -> HyphalTip -> Capital -> Maybe (HyphalTip, Capital)
executeTrade (Price p) agent (Capital walletBalance) =
    let
        genes = hypGenome agent
        step = hypStepCount agent
        maxOrders = geneMaxOrders genes
    in
        if step >= maxOrders
            then Nothing
            else
                let
                    volMult = if step == 0 then 1.0 else (geneVolMult genes) ^ step
                    d = calculateFractalDim (hypPath agent)
                    q_f = calculateFlowRate d
                    baseAmt = if step == 0 then geneBaseOrder genes else geneDCAOrder genes
                    orderCostVal = baseAmt * volMult * q_f
                    isAffordable = orderCostVal <= walletBalance
                in
                    if not isAffordable
                        then Nothing
                        else
                            let
                                orderCost = Capital orderCostVal
                                orderQty = Quantity (orderCostVal / p)
                                newPos = (hypHoldings agent) <> Position orderQty orderCost
                                newAgent = agent
                                    {
                                    hypHoldings = newPos,
                                    hypRefPrice = Price p,
                                    hypStepCount = step + 1
                                    }
                            in
                                Just (newAgent, orderCost)

moveAgent :: Double -> HyphalTip -> StdGen -> HyphalTip
moveAgent pressure agent rng =
    let
        genes = hypGenome agent
        psi_crit = geneTurbulence genes
        k = 0.5
        sigmoid = 1.0 / (1.0 + exp (-(k * (pressure - psi_crit))))
        currentLoc = hypLocation agent
        currentVel = hypVelocity agent
        (r1, rng1) = randomR (-1.0, 1.0) rng
        (r2, _) = randomR (-1.0, 1.0) rng1
        randomVec = [r1, r2]
        safeVel = if all (==0) currentVel then randomVec else currentVel
        newVel = zipWith (\v r -> (1.0 - sigmoid) * v + sigmoid * r) safeVel randomVec
        eta = geneGrowthRate genes
        newLoc = zipWith (\x v -> x + eta * v) currentLoc newVel
    in
        agent { hypLocation = newLoc, hypVelocity = newVel, hypPath = newLoc : hypPath agent }

-- ======================
-- MACRO DYNAMICS
-- ======================

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

spawnMushrooms :: Price -> Sim ()
spawnMushrooms currentPrice = do
    agents <- getAgents
    mushrooms <- getMushrooms
    let candidates = [hypLocation a | a <- agents]
    let newSpawns = filter (\loc -> calculateLocalField loc agents currentPrice > mushroomThreshold) candidates
    let validSpawns = filter (\loc -> all (\m -> euclideanDistance loc (mushLocation m) > sensingRadius) mushrooms) newSpawns
    
    let newMushroomBody = case validSpawns of
          [] -> []
          (loc:_) -> [MushroomBody
           {
           mushId = length mushrooms + 1,
           mushLocation = loc,
           mushMass = Capital 0.0,
           mushGenome = Genome 0.5 10.0 0.001 1000.0 0.1 10.0 20.0 5 1.1 1.2 5 1.0
           }]
    
    unless (null newMushroomBody) $ setMushrooms (mushrooms ++ newMushroomBody)

type TaxMap = [(Int, Capital)] 

applyDrain :: HyphalTip -> [MushroomBody] -> Price -> [HyphalTip] -> (HyphalTip, [(MushroomId, Capital)])
applyDrain agent mushrooms currentPrice allAgents =
  let
    pid = hypParentId agent
    parentMaybe = filter (\m -> mushId m == pid) mushrooms
  in
    case parentMaybe of
      [] -> (agent, [])
      (parent:_) ->
        let
          psi_i = calculatePressure currentPrice agent
          phi_m = calculateLocalField (mushLocation parent) allAgents currentPrice

          k_vac = geneVacuumCoeffient (mushGenome parent)
          vacuum = -(k_vac * phi_m)

          (Capital currentBank) = bioBank (hypBiology agent)
          isToxic = psi_i < vacuum

          (drainAmount, newBioBank) = if isToxic
            then (Capital currentBank, Capital 0)
            else
              let
                tau = fromIntegral (bioAge (hypBiology agent)) :: Double
                flux = tau * (psi_i - vacuum)
                tax = max 0.0 flux
              in (Capital tax, Capital (currentBank - max 0.0 flux))

          newBio = (hypBiology agent) { bioBank = newBioBank }
          taxEntry = if drainAmount > 0 then [(pid, drainAmount)] else []
        in
          (agent {hypBiology = newBio }, taxEntry)


updateMushroom :: Price -> TaxMap -> StdGen -> MushroomBody -> (MushroomBody, [Spore])
updateMushroom (Price p) income rng mBody =
    let
        myIncome = sum [amt | (mid, amt) <- income, mid == mushId mBody]
        massAfterIncome = (mushMass mBody) + myIncome
        genes = mushGenome mBody
        cost = Capital (geneMaintenance genes)
        massAfterCost = massAfterIncome - cost
        maturity = geneMaturity genes
        (Capital mVal) = massAfterCost
    in
        if mVal > maturity
            then
                let
                    maxChildren = max 1 (fromIntegral (geneMaxChildren genes))
                    
                    -- FIX: Base injection on MATURITY, not current mass
                    -- This ensures equal funding for all spores
                    injectionAmt = maturity / maxChildren 
                    sporeCost = Capital injectionAmt
                    
                    massAfterSpore = massAfterCost - sporeCost
                    (mutatedGenes) = mutateGenome genes rng
                    (r1, rng1) = randomR (-1.0, 1.0) rng
                    (r2, _) = randomR (-1.0, 1.0) rng1
                    dispersion = geneDispersion genes
                    target = zipWith (+) (mushLocation mBody) [r1 * dispersion, r2 * dispersion]
                    newSpore = Spore
                        { sporeTarget = target
                        , sporeGenome = mutatedGenes
                        , sporeCapital = sporeCost
                        , sporeTimer = 10
                        }
                    finalMushroom = mBody { mushMass = massAfterSpore }
                in
                    (finalMushroom, [newSpore])
            else
                (mBody { mushMass = massAfterCost }, [])

-- ... (updateHypha and stepSimulation logic remain identical, just ensure updated updateMushroom is called) ...

updateHypha :: Price -> [MushroomBody] -> [HyphalTip] -> HyphalTip -> Sim (Maybe HyphalTip, TaxMap)
updateHypha currentPrice mushrooms allAgents agent = do
    let (agentAfterTax, taxes) = applyDrain agent mushrooms currentPrice allAgents
    let psi = calculatePressure currentPrice agentAfterTax
    let dieThresh = -50.0
    let (Capital bank) = bioBank (hypBiology agentAfterTax)

    if psi < dieThresh || bank < 0
        then return (Nothing, taxes)
        else do
            let strategy = interpretStrategy (hypLocation agent)
            let genes = hypGenome agent
            let step = hypStepCount agent
            let volMult = if step == 0 then 1.0 else (geneVolMult genes) ^ step
            
            let shouldSell = shouldExecuteSell strategy currentPrice (hypHoldings agentAfterTax)
            
            agentAfterLogic <- if shouldSell
                then do
                    case executeSell currentPrice agentAfterTax of
                        Just (soldAgent, revenue, profit) -> do
                            modifyWallet (\c -> c + revenue)
                            return soldAgent
                        Nothing -> return agentAfterTax
                else do
                    let shouldBuy = shouldExecuteBuy strategy currentPrice (hypRefPrice agentAfterTax) volMult
                    if shouldBuy
                        then do
                            (GlobalWallet balance) <- getWallet
                            case executeTrade currentPrice agentAfterTax balance of
                                Just (boughtAgent, cost) -> do
                                    modifyWallet (\c -> c - cost)
                                    return boughtAgent
                                Nothing -> return agentAfterTax 
                        else return agentAfterTax

            t <- getTime
            let (Time tick) = t
            let rng = mkStdGen (hypId agent + tick * 1000)
            let agentAfterMove = moveAgent psi agentAfterLogic rng
            let bio = hypBiology agentAfterMove
            let finalAgent = agentAfterMove { hypBiology = bio { bioAge = bioAge bio + 1 } }

            return (Just finalAgent, taxes)

stepSimulation :: Price -> Sim ()
stepSimulation newPrice = do
    agents <- getAgents
    mushrooms <- getMushrooms
    spores <- getSpores
    time <- getTime
    
    modify $ \s -> s { sysEnv = (sysEnv s) { mktPrice = newPrice } }
    
    results <- mapM (updateHypha newPrice mushrooms agents) agents
    let survivingAgents = catMaybes (map fst results)
    let allTaxes = concat (map snd results)
    setAgents survivingAgents
    
    let (Time tInt) = time
    let processMushroom (mList, sList) m = 
          let (mNew, newSpores) = updateMushroom newPrice allTaxes (mkStdGen (mushId m + tInt)) m
          in if mushMass mNew > 0 
             then (mNew : mList, newSpores ++ sList) 
             else (mList, sList)
    
    let (livingMushrooms, newSpores) = foldl' processMushroom ([], []) mushrooms
    setMushrooms livingMushrooms
    
    let agedSpores = map (\s -> s { sporeTimer = sporeTimer s - 1 }) (spores ++ newSpores)
    let (germinating, dormant) = partition (\s -> sporeTimer s <= 0) agedSpores
    setSpores dormant
    
    let nextId = length agents + 100
    let newAgents = zipWith (\s idx -> HyphalTip
            { hypId = nextId + idx
            , hypLocation = sporeTarget s
            , hypVelocity = [0,0]
            , hypPath = [sporeTarget s]
            , hypHoldings = mempty
            , hypBiology = BioState { bioAge = 0, bioBank = sporeCapital s }
            , hypGenome = sporeGenome s
            , hypRefPrice = newPrice
            , hypStepCount = 0
            }) germinating [0..]
            
    unless (null newAgents) $ setAgents (survivingAgents ++ newAgents)
    spawnMushrooms newPrice
    modify $ \s -> s { sysTime = sysTime s + 1 }

genesisState :: SystemState
genesisState = SystemState
    { sysTime      = Time 0
    , sysWallet    = GlobalWallet 10000.0
    , sysEnv       = Environment (Price 100.0) [] -- Pheromone Grid kept empty as unused
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
        geneVolMult = 1.2,
        geneMaxChildren = 5,
        geneMaintenance = 0.5
    }
    testPos    = Position (Quantity 1.0) (Capital 100.0)
    testBio    = BioState 10 (Capital 500.0)

    testAgent = HyphalTip
        { hypId       = 1
        , hypLocation = [0.05, 0.10]
        , hypVelocity = [0.0, 0.0]
        , hypPath     = [[0.05, 0.10]]
        , hypHoldings = testPos
        , hypBiology  = testBio
        , hypGenome   = testGenome
        , hypRefPrice = Price 100.0
        , hypStepCount = 0
        }

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
    putStrLn $ "spore count: " ++ show (length $ sysSpores finalState)
    
    mapM_ (\m -> putStrLn $ "Mushroom " ++ show (mushId m) ++ " Mass: " ++ show (mushMass m)) (sysMushrooms finalState)

main :: IO ()
main = runTest 5000
