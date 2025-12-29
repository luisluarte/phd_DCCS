module Simulation.Micro where

import MycelialState
import MycelialStrategy
import MycelialPhysics (calculateFlowRate)
import qualified Simulation.Types as T
import Simulation.Accessors hiding (Sim)

processMicroFlow :: HyphalTip -> Price -> T.Sim HyphalTip
processMicroFlow agent (Price p) = do
    let (Capital b) = bioBank (hypBiology agent)
        flow = calculateFlowRate (fromIntegral (bioAge (hypBiology agent)))
    return agent { hypBiology = (hypBiology agent) { bioBank = Capital (b + flow) } }

spawnHypha :: MushroomId -> HyphalId -> [Double] -> Genome -> Price -> HyphalTip
spawnHypha mid hid loc genes p =
    HyphalTip hid mid loc [0,0] [loc] (Position 0 0) (BioState 0 (Capital 10)) genes p 0

executeMicroTrade :: HyphalTip -> Price -> HyphalTip
executeMicroTrade agent (Price p) =
    let strat = interpretStrategy agent (Price p)
        pos = hypHoldings agent
        (Capital bank) = bioBank (hypBiology agent)
    in case strat of
        Buy | bank > 1.0 -> 
            agent { hypHoldings = pos <> Position (Quantity (1.0/p)) (Capital 1.0),
                    hypBiology = (hypBiology agent) { bioBank = Capital (bank - 1.0) } }
        Sell -> 
            let (Quantity q) = posQuantity pos
            in agent { hypHoldings = Position 0 0, 
                       hypBiology = (hypBiology agent) { bioBank = Capital (bank + q * p) } }
        _ -> agent
