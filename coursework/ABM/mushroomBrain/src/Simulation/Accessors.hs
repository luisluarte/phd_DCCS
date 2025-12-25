module Simulation.Accessors where

import MycelialState -- this is where my data types are
import Control.Monad.State (State, gets, modify)
-- State carries state s and produces value a
-- gets, returns the value inside the state contex
-- modify, takes transformation s -> s and updates the state


type Sim a = State SystemState a -- domain specific monad

-- reading Time state
getTime :: Sim Time
getTime = gets sysTime

-- reading price state
-- here we do composition
-- sysEnv returns the Environment record
-- then, mktPrice returns the market price inside Environment
-- then, gets returns a monadic action with the specfic a value
-- gets takes a function of type s -> a
getPrice :: Sim Price
getPrice = gets (mktPrice . sysEnv)

-- reading wallet state
getWallet :: Sim GlobalWallet
getWallet = gets sysWallet

-- high-order function
-- transform a Capital value into a new Capital value (this is the type signature)
-- this then returns Sim with () meaning that there's no result value
-- modify takes the current state to a new state
-- \s is the lambda that represent the current state
-- final we resolve the new value with (f c) where f
-- will typically is a substraction or addition
modifyWallet :: (Capital -> Capital) -> Sim ()
modifyWallet f = modify $ \s ->
    let (GlobalWallet c) = sysWallet s
    in s { sysWallet = GlobalWallet (f c) }

-- same getter but for HyphalTips
getAgents :: Sim [HyphalTip]
getAgents = gets sysHyphae

-- this takes a list of hyphaltips, our agents
-- and then return the world only through its side effects ()
-- the lambda function \s updates sysHyphae with the new agents
-- using modify changes the state
setAgents :: [HyphalTip] -> Sim ()
setAgents newAgents = modify $ \s -> s { sysHyphae = newAgents }

-- same for the list of mushroobodies
getMushrooms :: Sim [MushroomBody]
getMushrooms = gets sysMushrooms

-- same logic here, this is to perform
-- a change of sysMushrooms with newMushrooms
setMushrooms :: [MushroomBody] -> Sim ()
setMushrooms newMushrooms = modify $ \s -> s { sysMushrooms = newMushrooms }

-- bla bla same
getSpores :: Sim [Spore]
getSpores = gets sysSpores

-- bla bla same logic to replace spores
setSpores :: [Spore] -> Sim ()
setSpores newSpores = modify $ \s -> s { sysSpores = newSpores }