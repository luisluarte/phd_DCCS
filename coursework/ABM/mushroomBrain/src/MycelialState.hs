{-# LANGUAGE DeriveGeneric #-}
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

data SimConfig = SimConfig
    { cfgEnableMutation     :: Bool
    , cfgEnableIntelligence :: Bool
    , cfgSporeBatchSize     :: Int
    , cfgMaxOrders          :: Int
    , cfgMaxChildren        :: Int
    , cfgMaintenanceCost    :: Double
    , cfgInitMaturity       :: Double
    , cfgInitGreed          :: Double
    , cfgInitTurbulence     :: Double
    , cfgInitGrowthRate     :: Double
    , cfgInitPhiCritical    :: Double
    , cfgInitVacuumCoefficient :: Double
    , cfgInitDevMult        :: Double
    , cfgDispersionRadius   :: Double
    , cfgInitBaseOrder      :: Double
    , cfgDcaOrder           :: Double
    } deriving (Show, Generic)

instance FromJSON SimConfig; instance ToJSON SimConfig

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
    } deriving (Show, Generic, FromJSON, ToJSON)

data BioState = BioState
    { bioAge  :: Int
    , bioBank :: Capital
    } deriving (Show, Generic, FromJSON, ToJSON)

data Position = Position 
    { posQuantity :: Quantity
    , posCost     :: Capital    -- Restored: Cost is stored directly again
    } deriving (Show, Generic, FromJSON, ToJSON)

instance Semigroup Position where 
    (Position q1 c1) <> (Position q2) c2 = Position (q1 + q2) (c1 + c2)
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
    , hypRefPrice  :: Price     -- Restored: Reference Price instead of AvgEntry
    , hypStepCount :: Int
    } deriving (Show, Generic, FromJSON, ToJSON)
