{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MycelialState where

import GHC.Generics
import Data.Aeson
import qualified Data.Map.Strict as Map

-- | Type synonym for spatial and strategic vectors
type ParamVector = [Double]

-- | Price, Capital, and Quantity with arithmetic capabilities
newtype Price = Price Double 
    deriving (Show, Generic, Eq, Ord, FromJSON, ToJSON)

newtype Capital = Capital Double 
    deriving (Show, Generic, Eq, Ord, Num, Fractional, FromJSON, ToJSON)

newtype Quantity = Quantity Double 
    deriving (Show, Generic, Eq, Ord, Num, Fractional, FromJSON, ToJSON)

-- | Genome tracks the biological and strategic parameters
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
    } deriving (Show, Generic)

instance FromJSON Genome; instance ToJSON Genome

data BioState = BioState
    { bioAge  :: Int      -- Restored for MycelialPhysics compatibility
    , bioBank :: Capital
    } deriving (Show, Generic)

instance FromJSON BioState; instance ToJSON BioState

data Position = Position
    { posQuantity :: Quantity
    } deriving (Show, Generic)

instance FromJSON Position; instance ToJSON Position

instance Semigroup Position where
    (Position q1) <> (Position q2) = Position (q1 + q2)

instance Monoid Position where
    mempty = Position 0

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
    , hypAvgEntry  :: Price   -- Weighted Average Cost Basis
    , hypStepCount :: Int
    } deriving (Show, Generic)

instance FromJSON HyphalTip; instance ToJSON HyphalTip

data MushroomBody = MushroomBody
    { mushId      :: MushroomId
    , mushLocation :: [Double]
    , mushMass     :: Capital
    , mushGenome   :: Genome
    } deriving (Show, Generic)

instance FromJSON MushroomBody; instance ToJSON MushroomBody

data Spore = Spore
    { sporeGenome  :: Genome
    , sporeTarget  :: [Double]
    , sporeCapital :: Capital
    } deriving (Show, Generic)

instance FromJSON Spore; instance ToJSON Spore

data SimStats = SimStats
    { statTick         :: Int
    , statTotalWealth  :: Double
    , statMktPrice     :: Double
    , statPopSize      :: Int
    , statFractalDims  :: [Double]
    , statHoldings     :: [Double]
    , statBioBank      :: [Double]
    , statGeneGreed    :: [Double]
    , statGeneTurbulence :: [Double]
    , statGeneGrowthRate :: [Double]
    , statGeneBaseOrder  :: [Double]
    , statGenePhiCritical :: [Double]
    , statGeneReproductiveInvest :: [Double]
    , statGeneVacuumCoefficient :: [Double]
    , statGeneDevMult :: [Double]
    , statStratDrop    :: [Double]
    , statStratProfit  :: [Double]
    } deriving (Show, Generic)

instance FromJSON SimStats; instance ToJSON SimStats

data Environment = Environment
    { mktPrice :: Price
    , mktHistory :: [Price]
    } deriving (Show, Generic)

instance FromJSON Environment; instance ToJSON Environment

data GlobalWallet = GlobalWallet Capital deriving (Show, Generic)
instance FromJSON GlobalWallet; instance ToJSON GlobalWallet

data SystemState = SystemState
    { sysTime      :: Time
    , sysWallet    :: GlobalWallet
    , sysEnv       :: Environment
    , sysHyphae    :: [HyphalTip]
    , sysMushrooms :: [MushroomBody]
    , sysSpores    :: [Spore]
    , sysLogs      :: [String]
    , sysSnapshots :: [SimStats]
    } deriving (Show, Generic)

instance FromJSON SystemState; instance ToJSON SystemState

newtype Time = Time Int deriving (Show, Generic, Eq, Ord, Num, FromJSON, ToJSON)

-- | Helper to derive historical cost from weighted average
-- This allows Physics and Strategy modules to compute 'posCost' logic
calculatePosCost :: Position -> Price -> Capital
calculatePosCost (Position (Quantity q)) (Price p) = Capital (q * p)
