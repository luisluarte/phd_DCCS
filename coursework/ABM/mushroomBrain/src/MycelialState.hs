{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MycelialState where

-- ========================================================
-- PRIMITIVE WRAPPERS
-- ========================================================

newtype Time = Time Int 
  deriving (Show, Eq, Ord, Num)

newtype Price = Price Double 
  deriving (Show, Eq, Ord, Num, Fractional)

newtype Capital = Capital Double 
  deriving (Show, Eq, Ord, Num, Fractional)

newtype Quantity = Quantity Double 
  deriving (Show, Eq, Ord, Num, Fractional)

-- ADDED: Real, Integral to allow fromIntegral usage
newtype MushroomId = MushroomId Int
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype HyphalId = HyphalId Int
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

type ParamVector = [Double] 
type PheromoneIntensity = Double 
type PheromoneMap = [(ParamVector, PheromoneIntensity)]

-- ========================================================
-- SYSTEM STATE TUPLE: S(t)
-- ========================================================

data SystemState = SystemState
  {
  sysTime :: Time, 
  sysWallet :: GlobalWallet, 
  sysEnv :: Environment, 
  sysHyphae :: [HyphalTip], 
  sysMushrooms :: [MushroomBody], 
  sysSpores :: [Spore] 
  } deriving (Show)

-- ========================================================
-- COMPONENT DEFINITIONS
-- ========================================================

newtype GlobalWallet = GlobalWallet Capital
  deriving (Show, Eq, Num)

data Environment = Environment
  {
  mktPrice :: Price, 
  pheromoneGrid :: PheromoneMap 
  } deriving (Show)

-- ========================================================
-- THE GENOME (G)
-- ========================================================

data Genome = Genome
  {
  -- 1. PHYSICS TRAITS
  geneGreed :: Double,       -- beta_1
  geneTurbulence :: Double,  -- Psi_crit
  geneGrowthRate :: Double,  -- eta
  geneMaturity :: Double,    -- M_T
  geneDispersion :: Double,  -- sigma_dispersal

  -- 2. SOCIAL & REPRODUCTIVE TRAITS
  genePhiCritical :: Double,        -- Quorum Sensing
  geneVacuumCoefficient :: Double,  -- Tax Rate
  geneReproductiveInvest :: Double, -- Gamma (% Mass sacrifice)
  geneSporeBatchSize :: Int,        -- N_spore

  -- 3. STRATEGY TRAITS (DCA)
  geneBaseOrder :: Double,
  geneDCAOrder :: Double,
  geneMaxOrders :: Int,
  geneDevMult :: Double,
  geneVolMult :: Double,

  -- 4. COLONIAL TRAITS
  geneMaxChildren :: Int,    -- N_brood
  geneMaintenance :: Double  -- Cost per tick
  } deriving (Show, Eq) -- Added Eq mostly for consistency, though not strictly required by error

-- ========================================================
-- AGENT DEFINITIONS
-- ========================================================

data HyphalTip = HyphalTip
  {
  hypId :: HyphalId,
  hypParentId :: MushroomId, -- STRICT: Must have a parent
  hypLocation :: ParamVector,
  hypVelocity :: ParamVector,
  hypPath :: [ParamVector],
  hypHoldings :: Position,
  hypBiology :: BioState,
  hypGenome :: Genome,
  hypRefPrice :: Price,
  hypStepCount :: Int
  } deriving (Show)

data Position = Position
  {
  posQuantity :: Quantity,
  posCost :: Capital
  } deriving (Show, Eq)

instance Semigroup Position where
  (Position q1 c1) <> (Position q2 c2) = Position (q1+q2) (c1+c2)

instance Monoid Position where
  mempty = Position 0 0

data BioState = BioState
  {
  bioAge :: Int,
  bioBank :: Capital
  } deriving (Show)

data MushroomBody = MushroomBody
  {
  mushId :: MushroomId,
  mushLocation :: ParamVector,
  mushMass :: Capital,
  mushGenome :: Genome
  } deriving (Show)

data Spore = Spore
  {
  sporeTarget :: ParamVector,
  sporeGenome :: Genome,
  sporeCapital :: Capital
  } deriving (Show, Eq) -- FIXED: Added Eq here