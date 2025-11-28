module MycelialState where

-- ========================================================
-- PRIMITIVE WRAPPERS
-- ========================================================

newtype Time = Time Int -- our ticks are discrete (t)
  deriving (Show, Eq, Ord, Num)

newtype Price = Price Double -- Market price a given asset P(t)
  deriving (Show, Eq, Ord, Num, Fractional)

newtype Capital = Capital Double -- USDT amount
  deriving (Show, Eq, Ord, Num, Fractional)

newtype Quantity = Quantity Double -- Asset Amount (BTC or other)
  deriving (Show, Eq, Ord, Num, Fractional)

type ParamVector = [Double] -- a vector in a parameter space (of DCA agent)

type PheromoneIntensity = Double -- intensity or agent internal pressure Psi

-- the map is a list of points in the parameter space
-- and their associated intensity
type PheromoneMap = [(ParamVector, PheromoneIntensity)]

-- ========================================================
-- SYSTEM STATE TUPLE: S(t)
-- ========================================================

data SystemState = SystemState
  {
  sysTime :: Time, -- t: current time step
  sysWallet :: GlobalWallet, -- W(t): shared resource
  sysEnv :: Environment, -- E(t): context
  sysHyphae :: [HyphalTip], -- H(t): active agents
  sysMushrooms :: [MushroomBody], -- M(t): resource sinks
  sysSpores :: [Spore] -- S(t): dormant vectors
  } deriving (Show)

-- ========================================================
-- COMPONENTS DEFINITIONS (FORM THE SYSTEM STATE)
-- ========================================================

-- Global wallet (W)
-- represents the single metabolic resource pool
newtype GlobalWallet = GlobalWallet Capital
  deriving (Show, Eq, Num)

-- The Environment (E)
-- contains the exogenous signal (price) and the endogenous map (pheromones)
data Environment = Environment
  {
  mktPrice :: Price, -- P(t)
  pheromoneGrid :: PheromoneMap -- Phi(x, t): the spatial memory
  } deriving (Show)

-- The Genome (G)
-- the evolvable traits inherited with mutation
data Genome = Genome
  {
  geneGreed :: Double, -- beta_1
  geneTurbulence :: Double, -- Psi_crit
  geneGrowthRate :: Double, -- eta
  geneMaturity :: Double, -- M_T
  geneDispersion :: Double -- SD how far the spores go
  } deriving (Show)

-- ========================================================
-- AGENT DEFINITIONS
-- ========================================================

-- The hyphal tip (h e H)
-- this is the active trader
data HyphalTip = HyphalTip
  {
  hypId :: Int, -- the identification
  hypLocation :: ParamVector, -- x: current strategy
  hypVelocity :: ParamVector, -- v: growth vector (Intertia)
  hypPath :: [ParamVector], -- History (with this we compute fractal dimension D)
  hypHoldings :: Position, -- q, v_cost
  hypBiology :: BioState, -- tau (age), pi_bank (internal pressure)
  hypGenome :: Genome -- G: traits
  } deriving (Show)

-- helper structs for the hypha
data Position = Position
  {
  posQuantity :: Quantity,
  posCost :: Capital
  } deriving (Show)

data BioState = BioState
  {
  bioAge :: Int,
  bioBank :: Capital
  } deriving (Show)

-- The mushroom body (m)
-- the stationary sink
data MushroomBody = MushroomBody
  {
  mushId :: Int, -- the identification
  mushLocation :: ParamVector, -- x_fixed
  mushMass :: Capital, -- M_mass (accumulated capital)
  mushGenome :: Genome -- G: template for spores
  } deriving (Show)

-- The spore (S)
-- the dormant explorer
data Spore = Spore
  {
  sporeTarget :: ParamVector, -- x_target
  sporeGenome :: Genome, -- mutated genome
  sporeTimer :: Int -- t_germ (countdown)
  } deriving (Show)
