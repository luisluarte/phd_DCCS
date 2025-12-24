module Simulation.Lifecycle where

import MycelialState
import Simulation.Accesors
import Simulation.Micro (executeTrade, executeSell, moveAgent)
import Simulation.Macro (applyDrain, TaxMap)
import Simulation.Evolution (mutateGenome)
import MycelialPhysics (calculatePressure, clampVector)
import MycelialStrategy (interpretStrategy, TradingStrategy(..), shouldExecuteBuy, shouldExecuteSell)
import System.Random (StdGen, mkStdGen, randomR)
import Control.Monad.State (get)


-- constants (TODO: put it in config module)
dieThresh :: Double
dieThresh = -50.0


-- hyphal lifecycle (agent update loop)
updateHypha :: Price -> [MushroomBody] -> [HyphalTip] -> HyphalTip -> Sim (Maybe HyphalTip, TaxMap)
updateHypha currentPrice mushrooms allAgents agent = do
    let (agentAfterTax, taxes) = applyDrain agent mushrooms currentPrice allAgents
    let psi = calculatePressure currentPrice agentAfterTax
    let (Capital bank) = bioBank (hypBiology agentAfterTax) 

    if psi < dieThresh || bank < 0 -- this is a hyphae that's going to be removes
        then do
            modifyWallet (\c -> c + Capital bank) -- Capital bank is the constructor, so types are the same
            return (Nothing, taxes)
        else do
            let strategy = interpretStrategy (hypLocation agent)
            let genes = hypGenome agent
            -- FIXED: volMult removed as it was unused locally
            let devMult = if (hypStepCount agent) == 0 then 1.0 else (geneDevMult genes) ^ (hypStepCount agent)
            
            let shouldSell = shouldExecuteSell strategy currentPrice (hypHoldings agentAfterTax)
            
            agentAfterLogic <- if shouldSell
                then do
                    case executeSell currentPrice agentAfterTax of
                        Just (soldAgent, revenue, _) -> do -- FIXED: Ignore 'profit'
                            modifyWallet (\c -> c + revenue)
                            return soldAgent
                        Nothing -> return agentAfterTax
                else do
                    let shouldBuy = shouldExecuteBuy strategy currentPrice (hypRefPrice agentAfterTax) devMult
                    if shouldBuy
                        then do
                            (GlobalWallet balance) <- getWallet
                            case executeTrade currentPrice agentAfterTax balance of
                                Just (boughtAgent, cost) -> do
                                    modifyWallet (\c -> c - cost)
                                    return boughtAgent
                                Nothing -> return agentAfterTax 
                        else return agentAfterTax

            t <- getTime
            let (Time tick) = t
            let (HyphalId hid) = hypId agent
            let rng = mkStdGen (hid + tick * 1000)
            let agentAfterMove = moveAgent psi agentAfterLogic rng
            let bio = hypBiology agentAfterMove
            let finalAgent = agentAfterMove { hypBiology = bio { bioAge = bioAge bio + 1 } }

            return (Just finalAgent, taxes)
