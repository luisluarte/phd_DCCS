module Simulation.Lifecycle where

import MycelialState
import qualified Simulation.Types as T
import Simulation.Accessors hiding (Sim)
import MycelialPhysics (moveHypha)
import qualified Data.Map.Strict as Map

updateHypha :: Bool -> Price -> Map.Map MushroomId any -> [HyphalTip] -> HyphalTip -> T.Sim (Maybe HyphalTip, [any], Capital)
updateHypha intel p mushMap allA agent = do
    let bank = bioBank (hypBiology agent)
        maint = Capital (geneMaintenance (hypGenome agent))
    if bank <= maint 
        then return (Nothing, [], 0)
        else do
            let newLoc = moveHypha intel p mushMap allA agent
            return (Just (agent { hypLocation = newLoc }), [], maint)
