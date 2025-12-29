{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MycelialState where

import GHC.Generics
import Data.Aeson

type ParamVector = [Double]

-- 1. Use 'deriving newtype' for Num/Fractional/JSON to treat them as simple numbers
newtype Price = Price Double 
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (FromJSON, ToJSON)

newtype Capital = Capital Double 
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (Num, Fractional, FromJSON, ToJSON)

newtype Quantity = Quantity Double 
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (Num, Fractional, FromJSON, ToJSON)

newtype Time = Time Int 
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (Num, FromJSON, ToJSON)

-- 2. Use 'deriving anyclass' for JSON on complex Record types
data SimConfig = SimConfig
    { cfgEnableMutation        :: Bool
    , cfgEnableIntelligence    :: Bool
    , cfgSporeBatchSize        :: Int
    , cfgMaxOrders             :: Int
    , cfgMaxChildren           :: Int
    , cfgMaintenanceCost       :: Double
    , cfgInitMaturity          :: Double
    , cfgInitGreed             :: Double
    , cfgInitTurbulence        :: Double
    , cfgInitGrowthRate        :: Double
    , cfgInitPhiCritical       :: Double
    , cfgInitVacuumCoefficient :: Double
    , cfgInitReproductiveInvest:: Double
    , cfgInitDevMult           :: Double
    , cfgDispersionRadius      :: Double
    , cfgInitBaseOrder         :: Double
    , cfgDcaOrder              :: Double
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

data Genome = Genome
    { geneGreed              :: Double
    , geneTurbulence         :: Double
    , geneGrowthRate         :: Double
    , geneMaturity           :: Double
    , geneDispersion         :: Double
    , geneMaintenance        :: Double
    , genePhiCritical        :: Double
    , geneVacuumCoefficient  :: Double
    , geneReproductiveInvest :: Double
    , geneSporeBatchSize     :: Int
    , geneBaseOrder          :: Double
    , geneDCAOrder           :: Double
    , geneMaxOrders          :: Int
    , geneDevMult            :: Double
    , geneVolMult            :: Double
    , geneMaxChildren        :: Int
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

data BioState = BioState
    { bioAge  :: Int
    , bioBank :: Capital
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

data Position = Position 
    { posQuantity :: Quantity
    , posCost     :: Capital 
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

-- Helper function required by Loop.hs
calculatePosCost :: Position -> Capital
calculatePosCost = posCost

instance Semigroup Position where 
    (Position q1 c1) <> (Position q2 c2) = Position (q1 + q2) (c1 + c2)
instance Monoid Position where 
    mempty = Position 0 0

newtype HyphalId = HyphalId Int 
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (FromJSON, ToJSON)

newtype MushroomId = MushroomId Int 
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (FromJSON, ToJSON)

data HyphalTip = HyphalTip
    { hypId        :: HyphalId
    , hypParentId  :: MushroomId
    , hypLocation  :: [Double]
    , hypVelocity  :: [Double]
    , hypPath      :: [[Double]]
    , hypHoldings  :: Position
    , hypBiology   :: BioState
    , hypGenome    :: Genome
    , hypRefPrice  :: Price
    , hypStepCount :: Int
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

data MushroomBody = MushroomBody
    { mushId       :: MushroomId
    , mushLocation :: [Double]
    , mushMass     :: Capital
    , mushGenome   :: Genome
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

data Spore = Spore
    { sporeTarget  :: [Double]
    , sporeGenome  :: Genome
    , sporeCapital :: Capital
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

newtype GlobalWallet = GlobalWallet Capital 
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (Num, FromJSON, ToJSON)

data Environment = Environment
    { mktPrice     :: Price
    , mktHistory   :: [Price]
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

data SimStats = SimStats
    { statTick                   :: Int
    , statTotalWealth            :: Double
    , statMktPrice               :: Double
    , statPopSize                :: Int
    , statMushroomCount          :: Int
    , statMushroomMasses         :: [Double] -- <--- NEW FIELD
    , statFractalDims            :: [Double]
    , statHoldings               :: [Double]
    , statBioBank                :: [Double]
    , statGeneGreed              :: [Double]
    , statGeneTurbulence         :: [Double]
    , statGeneGrowthRate         :: [Double]
    , statGeneBaseOrder          :: [Double]
    , statGenePhiCritical        :: [Double]
    , statGeneReproductiveInvest :: [Double]
    , statGeneVacuumCoefficient  :: [Double]
    , statGeneDevMult            :: [Double]
    , statStratDrop              :: [Double]
    , statStratProfit            :: [Double]
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

data SystemState = SystemState
    { sysTime      :: Time
    , sysWallet    :: GlobalWallet
    , sysEnv       :: Environment
    , sysHyphae    :: [HyphalTip]
    , sysMushrooms :: [MushroomBody]
    , sysSpores    :: [Spore]
    , sysLogs      :: [String]
    , sysSnapshots :: [SimStats]
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

-- ========================================================
-- IO PAYLOADS
-- ========================================================

data InputPayload = InputPayload
    { inputPrices :: [Double]
    , inputConfig :: SimConfig
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

data OutputPayload = OutputPayload
    { outputEquityCurve :: [Double]
    , outputStats       :: [SimStats]
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)
