module MycelialSimulation where

import MycelialState
import MycelialPhysics
import MycelialStrategy
import System.Random (StdGen, mkStdGen, randomR)
-- FIXED: Removed unused imports
import Control.Monad (replicateM_, when, unless)
import Control.Monad.State.Strict (State, get, put, gets, modify, runState, execState)
import Data.Maybe (catMaybes)
import Data.List (partition, foldl', mapAccumL)
-- Data.Ord and Data.List.sortBy were unused

type Sim a = State SystemState a

-- ======================
-- STATE ACCESSORS
-- ======================

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
sensingRadius = 0.20

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
        (r8, _)    = mutateFloat (geneDevMult g) 0.05 rng7 -- FIXED: Ignore unused rng8
    in
        g { geneGreed = min 0.99 r1
          , geneTurbulence = r2
          , geneGrowthRate = r3
          , geneBaseOrder = r4
          , genePhiCritical = r5
          , geneReproductiveInvest = min 0.9 (max 0.1 r6)
          , geneVacuumCoefficient = min 1.0 (max 0.01 r7)
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
        
        rawLoc = zipWith (\x v -> x + eta * v) currentLoc newVel
        newLoc = clampVector rawLoc
    in
        agent { hypLocation = newLoc, hypVelocity = newVel, hypPath = newLoc : hypPath agent }

-- ======================
-- MACRO DYNAMICS
-- ======================

-- UPDATED: Now accepts Mushrooms as field sources too!
-- This ensures spores landing near a wealthy mushroom always sense "success"
calculateLocalField :: ParamVector -> [HyphalTip] -> [MushroomBody] -> Price -> Double
calculateLocalField loc agents mushrooms currentPrice =
  let
    sigma = sensingRadius
    kernel r = exp (-(r**2) / (2 * sigma**2))
    
    -- 1. Agent Contributions (Worker Pheromones)
    agentContribs = map (\a -> 
      let 
        dist = euclideanDistance loc (hypLocation a)
        pressure = calculatePressure currentPrice a
      in if dist < (sensingRadius * 5.0) 
        then pressure * kernel dist
        else 0
        ) agents

    -- 2. Mushroom Contributions (Mother Pheromones)
    -- Mushrooms emit pressure proportional to their Mass (Capital)
    mushContribs = map (\m -> 
      let
          dist = euclideanDistance loc (mushLocation m)
          (Capital mass) = mushMass m
          -- Heuristic: Pressure = Mass / 10. A 2500 mass mushroom emits 250 pressure.
          pressure = mass / 10.0 
      in if dist < (sensingRadius * 5.0)
         then pressure * kernel dist
         else 0
      ) mushrooms

  in
    sum agentContribs + sum mushContribs

type TaxMap = [(MushroomId, Capital)]

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
          
          -- Updated: Pass mushrooms to field calc
          phi_m = calculateLocalField (mushLocation parent) allAgents mushrooms currentPrice

          k_vac = geneVacuumCoefficient (mushGenome parent)
          vacuum = -(k_vac * phi_m)

          (Capital currentBank) = bioBank (hypBiology agent)
          isToxic = psi_i < vacuum

          (drainAmount, newBioBank) = if isToxic
            then (Capital currentBank, Capital 0)
            else
              let
                tau = fromIntegral (bioAge (hypBiology agent)) :: Double
                flux = tau * (psi_i - vacuum)
                rawTax = max 0.0 flux
                cappedTax = min rawTax currentBank
              in (Capital cappedTax, Capital (currentBank - cappedTax))

          newBio = (hypBiology agent) { bioBank = newBioBank }
          taxEntry = if drainAmount > 0 then [(pid, drainAmount)] else []
        in
          (agent {hypBiology = newBio }, taxEntry)

updateMushroom :: Price -> TaxMap -> StdGen -> MushroomBody -> (MushroomBody, [Spore], Capital)
updateMushroom (Price _) income rng mBody = -- FIXED: Ignore price 'p'
    let
        myIncome = sum [amt | (mid, amt) <- income, mid == mushId mBody]
        massAfterIncome = (mushMass mBody) + myIncome
        genes = mushGenome mBody
        
        maintenanceCost = Capital (geneMaintenance genes)
        massAfterCost = massAfterIncome - maintenanceCost
        
        maturity = geneMaturity genes
        (Capital mVal) = massAfterCost
    in
        if mVal > maturity
            then
                let
                    gamma = geneReproductiveInvest genes
                    batchSize = geneSporeBatchSize genes

                    totalSacrifice = mVal * gamma
                    perSporeEndowment = totalSacrifice / fromIntegral batchSize
                    massAfterSporulation = massAfterCost - Capital totalSacrifice
                    (MushroomId midInt) = mushId mBody

                    generateSpore currentRng i =
                        let
                            seed = i * 13 + round mVal + (midInt * 7918)
                            (mutatedGenes) = mutateGenome genes (mkStdGen seed)
                            
                            (r1, rng1) = randomR (-1.0, 1.0) currentRng
                            (r2, rng2) = randomR (-1.0, 1.0) rng1
                            
                            disp = geneDispersion genes
                            target = zipWith (+) (mushLocation mBody) [r1 * disp, r2 * disp]
                            clampedTarget = clampVector target
                            
                            spore = Spore
                                { sporeTarget = clampedTarget
                                , sporeGenome = mutatedGenes
                                , sporeCapital = Capital perSporeEndowment
                                }
                        in
                            (rng2, spore)

                    (_, newSpores) = mapAccumL generateSpore rng [1..batchSize]

                    finalMushroom = mBody { mushMass = massAfterSporulation }
                in
                    (finalMushroom, newSpores, maintenanceCost)
            else
                (mBody { mushMass = massAfterCost }, [], maintenanceCost)


germinateColony :: MushroomId -> HyphalId -> Spore -> Price -> (MushroomBody, [HyphalTip])
germinateColony mid (HyphalId startAid) spore currentPrice =
    let
        genes = sporeGenome spore
        loc = sporeTarget spore
        (Capital totalCap) = sporeCapital spore

        nChildren = max 1 (geneMaxChildren genes)
        divisor = fromIntegral nChildren + 1.0
        shareSize = totalCap / divisor

        newMushroom = MushroomBody
            { mushId = mid
            , mushLocation = loc
            , mushMass = Capital shareSize
            , mushGenome = genes
        }

        createWorker i = HyphalTip
            { hypId = HyphalId (startAid + i)
            , hypParentId = mid
            , hypLocation = loc
            , hypVelocity = [0,0]
            , hypPath = [loc]
            , hypHoldings = mempty
            , hypBiology = BioState { bioAge = 0, bioBank = Capital shareSize }
            , hypGenome = genes
            , hypRefPrice = currentPrice
            , hypStepCount = 0
        }

        newHyphae = map createWorker [0..(nChildren - 1)]

    in
        (newMushroom, newHyphae)


-- Helper to filter spores that are too close to existing structures
filterCrowded :: [Spore] -> [MushroomBody] -> Double -> [Spore]
filterCrowded candidates existing radius =
    let 
        initialLocs = map mushLocation existing
        (finalKept, _) = foldl' (\(kept, lockedLocs) spore -> 
            let 
                loc = sporeTarget spore
                isTooClose = any (\l -> euclideanDistance loc l < radius) lockedLocs
            in 
                if isTooClose 
                then (kept, lockedLocs) 
                else (spore:kept, loc:lockedLocs) 
            ) ([], initialLocs) candidates
    in
        reverse finalKept


updateHypha :: Price -> [MushroomBody] -> [HyphalTip] -> HyphalTip -> Sim (Maybe HyphalTip, TaxMap)
updateHypha currentPrice mushrooms allAgents agent = do
    let (agentAfterTax, taxes) = applyDrain agent mushrooms currentPrice allAgents
    let psi = calculatePressure currentPrice agentAfterTax
    let (Capital bank) = bioBank (hypBiology agentAfterTax) 

    if psi < dieThresh || bank < 0
        then do
            modifyWallet (\c -> c + Capital bank)
            return (Nothing, taxes)
        else do
            let strategy = interpretStrategy (hypLocation agent)
            let genes = hypGenome agent
            -- FIXED: volMult removed as it was unused locally
            let devMult = if (hypStepCount agent) == 0 then 1.0 else (geneDevMult genes) ^ (hypStepCount agent)
            
            let shouldSell = shouldExecuteSell strategy currentPrice (hypHoldings agentAfterTax)
            
            agentAfterLogic <- if shouldSell
                then do
                    case executeSell currentPrice agentAfterTax of
                        Just (soldAgent, revenue, _) -> do -- FIXED: Ignore 'profit'
                            modifyWallet (\c -> c + revenue)
                            return soldAgent
                        Nothing -> return agentAfterTax
                else do
                    let shouldBuy = shouldExecuteBuy strategy currentPrice (hypRefPrice agentAfterTax) devMult
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
            let (HyphalId hid) = hypId agent
            let rng = mkStdGen (hid + tick * 1000)
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
    
    -- 1. AGENT PASS
    results <- mapM (updateHypha newPrice mushrooms agents) agents
    let survivingAgents = catMaybes (map fst results)
    let allTaxes = concat (map snd results)

    -- 2. MUSHROOM PASS
    let (Time tInt) = time
    let processMushroom (mList, sList, recycledTotal) m =
          let 
             (MushroomId midInt) = mushId m
             seed = midInt + tInt
             rng = mkStdGen seed
             (mNew, newSpores, maintPaid) = updateMushroom newPrice allTaxes rng m
          in 
             if mushMass mNew > 0
             then (mNew : mList, newSpores ++ sList, recycledTotal + maintPaid)
             else 
                let deadMass = mushMass mNew
                in (mList, sList, recycledTotal + maintPaid + max (Capital 0.0) deadMass)
    
    let (livingMushrooms, newlyReleasedSpores, totalRecycled) = foldl' processMushroom ([], [], Capital 0.0) mushrooms
    
    modifyWallet (\c -> c + totalRecycled)

    -- 3. CASCADE DEATH
    let survivorIds = [mushId m | m <- livingMushrooms]
    let (orphans, keptAgents) = partition (\a -> not ((hypParentId a) `elem` survivorIds)) survivingAgents
    
    let orphanCash = sum [c | a <- orphans, let (Capital c) = bioBank (hypBiology a)]
    modifyWallet (\c -> c + Capital orphanCash)

    -- 4. SPORE PASS
    let allSpores = spores ++ newlyReleasedSpores

    -- UPDATED: Pass mushrooms to checkQuorum field calc
    let checkQuorum s =
          let
              loc = sporeTarget s
              -- FIXED: Include mushrooms in the field calculation
              field = calculateLocalField loc keptAgents livingMushrooms newPrice
              threshold = genePhiCritical (sporeGenome s)
          in
              field > threshold

    let (potentialColonizers, failures) = partition checkQuorum allSpores

    let exclusionRadius = sensingRadius * 0.25 
    
    let colonizers = filterCrowded potentialColonizers livingMushrooms exclusionRadius

    let crowdedOut = filter (\s -> not (s `elem` colonizers)) potentialColonizers
    let totalFailures = failures ++ crowdedOut

    let recycleAmount = sum [c | (Spore _ _ (Capital c)) <- totalFailures]
    let fedMushrooms = if not (null livingMushrooms) && recycleAmount > 0
          then
              let
                  share = recycleAmount / fromIntegral (length livingMushrooms)
                  feed m = m { mushMass = mushMass m + Capital share }
              in
                  map feed livingMushrooms
          else livingMushrooms

    let maxMid = if null fedMushrooms then 0 else maximum [i | (MushroomBody (MushroomId i) _ _ _) <- fedMushrooms]
    let maxHid = if null keptAgents then 0 else maximum [i | (HyphalTip (HyphalId i ) _ _ _ _ _ _ _ _ _) <- keptAgents]

    let startMid = maxMid + 1
    let startHid = maxHid + 1

    let processSpore (mList, aList, nextMid, nextHid) spore =
          let
              (newM, newAs) = germinateColony (MushroomId nextMid) (HyphalId nextHid) spore newPrice
              count = length newAs
          in
              (newM : mList, newAs ++ aList, nextMid + 1, nextHid + count)

    let (newColonies, newWorkers, _, _) = 
            foldl' processSpore ([], [], startMid, startHid) colonizers

    -- 5. COMMIT
    setSpores []
    setMushrooms (fedMushrooms ++ newColonies)
    setAgents (keptAgents ++ newWorkers)

    modify $ \s -> s { sysTime = sysTime s + 1 }

-- ========================================================
-- GENESIS CONFIGURATION (RESTORED)
-- ========================================================
-- This was missing in your last build, causing "Variable not in scope" errors.

genesisState :: SystemState
genesisState = SystemState
    { sysTime      = Time 0
    , sysWallet    = GlobalWallet 10000.0
    , sysEnv       = Environment (Price 100.0) [] 
    , sysHyphae    = initialAgents 
    , sysMushrooms = [genesisMushroom]
    , sysSpores    = []
    }
  where
    genesisGenome = Genome
        {
        geneGreed = 0.5,
        geneTurbulence = 2.0,
        geneGrowthRate = 0.01,
        
        geneMaturity = 500.0,
        geneDispersion = 0.25,
        geneMaintenance = 0.2,

        genePhiCritical = 1.0,
        geneVacuumCoefficient = 0.2,
        geneReproductiveInvest = 0.2,
        geneSporeBatchSize = 5,

        geneBaseOrder = 200.0,
        geneDCAOrder = 200.0,
        geneMaxOrders = 5,
        geneDevMult = 1.0,
        geneVolMult = 1.0,
        geneMaxChildren = 5
        }

    genesisMushroom = MushroomBody
        { mushId = MushroomId 1
        , mushLocation = [0.03, 0.05] 
        , mushMass = Capital 1000.0
        , mushGenome = genesisGenome
        }

    initialAgents = 
        [ HyphalTip
            { hypId = HyphalId i
            , hypParentId = MushroomId 1
            , hypLocation = [0.03 + (fromIntegral i * 0.001), 0.05 + (fromIntegral i * 0.001)] 
            , hypVelocity = [0.001 * fromIntegral (i `mod` 3 - 1), 0.001 * fromIntegral (i `mod` 2 - 1)]
            , hypPath     = [[0.03, 0.05]]
            , hypHoldings = Position (Quantity 0.0) (Capital 0.0)
            , hypBiology  = BioState 0 (Capital 200.0)
            , hypGenome   = genesisGenome
            , hypRefPrice = Price 100.0
            , hypStepCount = 0
            }
        | i <- [1..10]
        ]