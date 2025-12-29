{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-} -- Required for JSON
module MycelialState where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- ========================================================
-- 1. CONFIGURATION (Input from R)
-- ========================================================

data SimConfig = SimConfig
  { -- A. Simulation Control
    cfgEnableMutation     :: Bool
  , cfgEnableIntelligence :: Bool 
  , cfgSporeBatchSize     :: Int

  -- B. Fixed System Parameters
  , cfgDcaOrder           :: Double 
  , cfgMaxOrders          :: Int    
  , cfgMaxChildren        :: Int    
  , cfgDispersionRadius   :: Double 
  , cfgMaintenanceCost    :: Double 
  
  -- C. Genesis Genome (Starting Values for Mutating Genes)
  , cfgInitGreed              :: Double
  , cfgInitTurbulence         :: Double
  , cfgInitGrowthRate         :: Double
  , cfgInitBaseOrder          :: Double
  , cfgInitPhiCritical        :: Double
  , cfgInitReproductiveInvest :: Double
  , cfgInitVacuumCoefficient  :: Double
  , cfgInitDevMult            :: Double
  } deriving (Show, Generic)

instance FromJSON SimConfig
instance ToJSON SimConfig

data InputPayload = InputPayload
  { inputPrices :: [Double]
  , inputConfig :: SimConfig
  } deriving (Show, Generic)

instance FromJSON InputPayload
instance ToJSON InputPayload

-- ========================================================
-- PRIMITIVE WRAPPERS
-- ========================================================

newtype Time = Time Int 
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral, Generic)

instance ToJSON Time
instance FromJSON Time

newtype Price = Price Double 
  deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)

instance ToJSON Price
instance FromJSON Price

newtype Capital = Capital Double 
  deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)

instance ToJSON Capital
instance FromJSON Capital

newtype Quantity = Quantity Double 
  deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)

instance ToJSON Quantity
instance FromJSON Quantity

newtype MushroomId = MushroomId Int
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Generic)

instance ToJSON MushroomId
instance FromJSON MushroomId

newtype HyphalId = HyphalId Int
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Generic)

instance ToJSON HyphalId
instance FromJSON HyphalId

type ParamVector = [Double] 
type PheromoneIntensity = Double 
type PheromoneMap = [(ParamVector, PheromoneIntensity)]

-- ========================================================
-- LOGGING & STATS TYPES
-- ========================================================

data TransactionType = ActionBuy | ActionSell 
  deriving (Show, Eq, Generic)

instance ToJSON TransactionType

data TransactionLog = TransactionLog
  { tlHyphaId :: !HyphalId
  , tlType :: !TransactionType
  , tlCost :: !Capital      
  , tlPrice :: !Price       
  , tlQuantity :: !Quantity 
  , tlTime :: !Time         
  } deriving (Show, Generic)

instance ToJSON TransactionLog

-- COMPREHENSIVE STATS FOR R ANALYSIS
data SimStats = SimStats
  { statTick          :: Int
  , statTotalWealth   :: Double
  , statMktPrice      :: Double
  , statPopSize       :: Int
  
  -- BEHAVIORAL STATE
  , statFractalDims   :: [Double]
  , statHoldings      :: [Double]
  , statBioBank       :: [Double]

  -- EVOLUTIONARY STATE (The 8 Mutating Genes)
  , statGeneGreed              :: [Double]
  , statGeneTurbulence         :: [Double]
  , statGeneGrowthRate         :: [Double]
  , statGeneBaseOrder          :: [Double]
  , statGenePhiCritical        :: [Double]
  , statGeneReproductiveInvest :: [Double]
  , statGeneVacuumCoefficient  :: [Double]
  , statGeneDevMult            :: [Double]
  } deriving (Show, Generic)

instance ToJSON SimStats

data OutputPayload = OutputPayload
  { outputEquityCurve :: [Double]
  , outputStats       :: [SimStats] 
  } deriving (Show, Generic)

instance ToJSON OutputPayload

-- Legacy Snapshot (Keep for internal compatibility if referenced)
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
  -- We now store SimStats in the history for JSON output
  sysSnapshots :: ![SimStats] 
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
  } deriving (Show, Eq, Generic) 

instance ToJSON Genome
instance FromJSON Genome

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