{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MycelialState where

import GHC.Generics
import Data.Aeson

-- ========================================================
-- CORE TYPES
-- ========================================================

newtype Capital = Capital Double 
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (Num, Fractional, FromJSON, ToJSON)

-- | Configuration for the Search
data SimConfig = SimConfig
    { cfgNumAgents :: Int    -- Number of probes
    , cfgMaxStep   :: Double -- Speed of movement
    , cfgMaxLag    :: Int    -- Boundary of the world (e.g., 100 ticks back)
    , cfgSimilarityThreshold :: Double -- How strict the pattern matching is (Sigma)
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

-- | The Hypha (Probe in Hypothesis Space)
data HyphalTip = HyphalTip
    { hypId      :: Int
    , hypLoc     :: (Double, Double) -- (Lag1, Lag2) - Floating point for smooth movement
    , hypBiomass :: Capital          -- Energy (accumulated prediction accuracy)
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

-- | The World State
data SystemState = SystemState
    { sysTime    :: Int
    , sysHistory :: [Double]    -- The Price Series (The Territory)
    , sysAgents  :: [HyphalTip] -- The Population
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)

-- ========================================================
-- OUTPUT
-- ========================================================

data OutputPayload = OutputPayload
    { outputAgents :: [(Double, Double, Double)] -- (x, y, biomass)
    , outputTick   :: Int
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON, ToJSON)
