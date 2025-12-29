module MycelialSimulation 
  ( -- Re-exports
    module Simulation.Types
  , module Simulation.Accessors
  , module Simulation.Evolution
  , module Simulation.Micro
  , module Simulation.Macro
  , module Simulation.Lifecycle
  , module Simulation.Loop
  
  -- Export the Runner
  , runSeq
  ) where

import MycelialState (SystemState, Price, SimConfig)
import Simulation.Types (Sim)
import Simulation.Accessors hiding (Sim)
import Simulation.Evolution
import Simulation.Micro
import Simulation.Macro
import Simulation.Lifecycle
import Simulation.Loop (stepSimulation, genesisState) -- Import stepSimulation

import Control.Monad.State (execState)
import Data.Foldable (mapM_) -- Import standard mapM_

-- | Run a sequence of prices with a specific configuration
-- This replaces the old local 'foldl' version in Main.hs
runSeq :: SimConfig -> [Price] -> SystemState -> SystemState
runSeq config prices initialState = 
    -- We map stepSimulation over the list of prices.
    -- execState runs this sequence on the initialState and returns the final state.
    execState (mapM_ (stepSimulation config) prices) initialState