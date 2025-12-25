{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MycelialState where

-- ========================================================
-- PRIMITIVE WRAPPERS (Fixed: No '!' allowed here)
-- ========================================================

newtype Time = Time Int 
  deriving (Show, Eq, Ord, Num)

newtype Price = Price Double 
  deriving (Show, Eq, Ord, Num, Fractional)

newtype Capital = Capital Double 
  deriving (Show, Eq, Ord, Num, Fractional)

newtype Quantity = Quantity Double 
  deriving (Show, Eq, Ord, Num, Fractional)

newtype MushroomId = MushroomId Int
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype HyphalId = HyphalId Int
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

type ParamVector = [Double] 
type PheromoneIntensity = Double 
type PheromoneMap = [(ParamVector, PheromoneIntensity)]

-- ========================================================
-- LOGGING TYPES (Strict Fields)
-- ========================================================

data TransactionType = ActionBuy | ActionSell 
  deriving (Show, Eq)

-- Strictness (!) IS allowed and recommended here
data TransactionLog = TransactionLog
  { tlHyphaId :: !HyphalId
  , tlType :: !TransactionType
  , tlCost :: !Capital      
  , tlPrice :: !Price       
  , tlQuantity :: !Quantity 
  , tlTime :: !Time         
  } deriving (Show)

data SystemSnapshot = SystemSnapshot
  { snapTime :: !Time
  , snapMarketPrice :: !Price
  , snapTotalCash :: !Capital       
  , snapInventoryValue :: !Capital  
  , snapMushroomMass :: !Capital    
  , snapMeanFractalDim :: !Double   
  , snapTotalWealth :: !Capital     
  } deriving (Show)

-- ========================================================
-- SYSTEM STATE
-- ========================================================

data SystemState = SystemState
  {
  sysTime :: !Time, 
  sysWallet :: !GlobalWallet, 
  sysEnv :: !Environment, 
  sysHyphae :: ![HyphalTip], 
  sysMushrooms :: ![MushroomBody], 
  sysSpores :: ![Spore],
  sysLogs :: ![TransactionLog],    
  sysSnapshots :: ![SystemSnapshot] 
  } deriving (Show)

-- ========================================================
-- COMPONENTS
-- ========================================================

newtype GlobalWallet = GlobalWallet Capital
  deriving (Show, Eq, Num)

data Environment = Environment
  {
  mktPrice :: !Price, 
  pheromoneGrid :: !PheromoneMap 
  } deriving (Show)

-- ========================================================
-- GENOME
-- ========================================================

data Genome = Genome
  {
  geneGreed :: !Double,
  geneTurbulence :: !Double,
  geneGrowthRate :: !Double,
  geneMaturity :: !Double,
  geneDispersion :: !Double,
  genePhiCritical :: !Double,
  geneVacuumCoefficient :: !Double,
  geneReproductiveInvest :: !Double,
  geneSporeBatchSize :: !Int,
  geneBaseOrder :: !Double,
  geneDCAOrder :: !Double,
  geneMaxOrders :: !Int,
  geneDevMult :: !Double,
  geneVolMult :: !Double,
  geneMaxChildren :: !Int,
  geneMaintenance :: !Double
  } deriving (Show, Eq) 

-- ========================================================
-- AGENTS
-- ========================================================

data HyphalTip = HyphalTip
  {
  hypId :: !HyphalId,
  hypParentId :: !MushroomId,
  hypLocation :: !ParamVector,
  hypVelocity :: !ParamVector,
  hypPath :: ![ParamVector],
  hypHoldings :: !Position,
  hypBiology :: !BioState,
  hypGenome :: !Genome,
  hypRefPrice :: !Price,
  hypStepCount :: !Int
  } deriving (Show)

data Position = Position
  {
  posQuantity :: !Quantity,
  posCost :: !Capital
  } deriving (Show, Eq)

instance Semigroup Position where
  (Position q1 c1) <> (Position q2 c2) = Position (q1+q2) (c1+c2)

instance Monoid Position where
  mempty = Position 0 0

data BioState = BioState
  {
  bioAge :: !Int,
  bioBank :: !Capital
  } deriving (Show)

data MushroomBody = MushroomBody
  {
  mushId :: !MushroomId,
  mushLocation :: !ParamVector,
  mushMass :: !Capital,
  mushGenome :: !Genome
  } deriving (Show)

data Spore = Spore
  {
  sporeTarget :: !ParamVector,
  sporeGenome :: !Genome,
  sporeCapital :: !Capital
  } deriving (Show, Eq)