module Simulation.Types where

import MycelialState
import Control.Monad.State (State)

-- The Simulation Monad alias
-- This freezes the 's' parameter of the State monad to SystemState,
-- simplifying type signatures across all other modules.
type Sim a = State SystemState a