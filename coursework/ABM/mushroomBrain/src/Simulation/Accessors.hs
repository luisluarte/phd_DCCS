module Simulation.Accessors where

import MycelialState
import Control.Monad.State (gets, modify)
import Simulation.Types (Sim) -- IMPORT Sim instead of redefining it!

-- reading Time state
getTime :: Sim Time
getTime = gets sysTime

-- reading price state
getPrice :: Sim Price
getPrice = gets (mktPrice . sysEnv)

-- reading wallet state
getWallet :: Sim GlobalWallet
getWallet = gets sysWallet

modifyWallet :: (Capital -> Capital) -> Sim ()
modifyWallet f = modify $ \s ->
    let (GlobalWallet c) = sysWallet s
    in s { sysWallet = GlobalWallet (f c) }

getAgents :: Sim [HyphalTip]
getAgents = gets sysHyphae

setAgents :: [HyphalTip] -> Sim ()
setAgents newAgents = modify $ \s -> s { sysHyphae = newAgents }

getMushrooms :: Sim [MushroomBody]
getMushrooms = gets sysMushrooms

setMushrooms :: [MushroomBody] -> Sim ()
setMushrooms newMushrooms = modify $ \s -> s { sysMushrooms = newMushrooms }

getSpores :: Sim [Spore]
getSpores = gets sysSpores

setSpores :: [Spore] -> Sim ()
setSpores newSpores = modify $ \s -> s { sysSpores = newSpores }
