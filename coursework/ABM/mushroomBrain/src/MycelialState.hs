{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MycelialState where

import GHC.Generics
import Data.Aeson

type ParamVector = [Double]

newtype Price = Price Double deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON)
newtype Capital = Capital Double deriving (Show, Generic, Eq, Ord, Num, Fractional, FromJSON, ToJSON)
newtype Quantity = Quantity Double deriving (Show, Generic, Eq, Ord, Num, Fractional, FromJSON, ToJSON)
newtype Time = Time Int deriving (Show, Generic, Eq, Ord, Num, FromJSON, ToJSON)

-- ========================================================
-- CONFIGURATION & GENOME
-- ========================================================

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
    , cfgInitReproductiveInvest:: Double -- Added this missing field
    , cfgInitDevMult           :: Double
    , cfgDispersionRadius      :: Double
    , cfgInitBaseOrder         :: Double
    , cfgDcaOrder              :: Double
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

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
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

-- ========================================================
-- SIMULATION OBJECTS
-- ========================================================

data BioState = BioState
    { bioAge  :: Int
    , bioBank :: Capital
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

data Position = Position 
    { posQuantity :: Quantity
    , posCost     :: Capital 
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

-- Helper function required by Loop.hs
calculatePosCost :: Position -> Capital
calculatePosCost = posCost

instance Semigroup Position where 
    (Position q1 c1) <> (Position q2 c2) = Position (q1 + q2) (c1 + c2)
instance Monoid Position where 
    mempty = Position 0 0

newtype HyphalId = HyphalId Int deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON)
newtype MushroomId = MushroomId Int deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON)

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
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

data MushroomBody = MushroomBody
    { mushId       :: MushroomId
    , mushLocation :: [Double]
    , mushMass     :: Capital
    , mushGenome   :: Genome
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

data Spore = Spore
    { sporeTarget  :: [Double]
    , sporeGenome  :: Genome
    , sporeCapital :: Capital
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

newtype GlobalWallet = GlobalWallet Capital 
    deriving (Show, Generic, Eq, Ord, Num, FromJSON, ToJSON)

data Environment = Environment
    { mktPrice     :: Price
    , mktHistory   :: [Price]
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

data SimStats = SimStats
    { statTick                   :: Int
    , statTotalWealth            :: Double
    , statMktPrice               :: Double
    , statPopSize                :: Int
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
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

data SystemState = SystemState
    { sysTime      :: Time
    , sysWallet    :: GlobalWallet
    , sysEnv       :: Environment
    , sysHyphae    :: [HyphalTip]
    , sysMushrooms :: [MushroomBody]
    , sysSpores    :: [Spore]
    , sysLogs      :: [String]
    , sysSnapshots :: [SimStats]
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

-- ========================================================
-- IO PAYLOADS (Used by Main.hs)
-- ========================================================

data InputPayload = InputPayload
    { inputPrices :: [Double]
    , inputConfig :: SimConfig
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

data OutputPayload = OutputPayload
    { outputEquityCurve :: [Double]
    , outputStats       :: [SimStats]
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)
