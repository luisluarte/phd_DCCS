module Simulation.Macro where

import MycelialState
import MycelialPhysics (euclideanDistance, calculatePressure)
import Data.List (foldl')
import qualified Data.Map.Strict as Map 

sensingRadius :: Double
sensingRadius = 0.20

calculateLocalField :: ParamVector -> [HyphalTip] -> [MushroomBody] -> Price -> Double
calculateLocalField loc agents mushrooms currentPrice =
  let
    sigma = sensingRadius
    kernel r = exp (-(r**2) / (2 * sigma**2))
    
    agentContribs = map (\a -> 
      let 
        dist = euclideanDistance loc (hypLocation a)
        pressure = calculatePressure currentPrice a
      in if dist < (sensingRadius * 5.0) 
        then pressure * kernel dist
        else 0
        ) agents

    mushContribs = map (\m -> 
      let
          dist = euclideanDistance loc (mushLocation m)
          (Capital mass) = mushMass m
          pressure = mass / 10.0
      in if dist < (sensingRadius * 5.0)
         then pressure * kernel dist
         else 0
      ) mushrooms

  in
    sum agentContribs + sum mushContribs

type TaxMap = [(MushroomId, Capital)]
type MushroomCache = Map.Map MushroomId (MushroomBody, Double)

applyDrain :: HyphalTip -> MushroomCache -> Price -> (HyphalTip, TaxMap)
applyDrain agent mushCache currentPrice =
  let
    pid = hypParentId agent
    parentData = Map.lookup pid mushCache
  in
    case parentData of
      Nothing -> (agent, [])
      Just (parent, phi_m) -> 
        let
          -- RE-ENGINEERED PHYSICS:
          -- k_vac now acts as the CONDUCTIVITY SCALER (The Valve).
          k_vac = geneVacuumCoefficient (mushGenome parent)
        
        in if k_vac <= 1e-6 
           then (agent, []) -- Valve Closed
           else 
            let
              -- Target Vacuum is fixed relative to Parent Mass (phi_m)
              -- This represents the "Hunger" of the colony.
              vacuum = -1.0 * phi_m 
              
              psi_i = calculatePressure currentPrice agent
              (Capital currentBank) = bioBank (hypBiology agent)
              
              isToxic = psi_i < vacuum
              
              (drainAmount, newBioBank) = if isToxic
                then (Capital currentBank, Capital 0)
                else
                  let
                    tau = fromIntegral (bioAge (hypBiology agent)) :: Double
                    
                    -- NEW FORMULA: Flux = (Age * k_vac) * Gradient
                    -- Dampen factor 0.001 ensures k_vac=1.0 isn't instantly fatal.
                    conductivity = 0.001 * tau * k_vac
                    
                    flux = conductivity * (psi_i - vacuum)
                    
                    -- Safety: Max 10% of bank per tick
                    rawTax = max 0.0 flux
                    safeTax = min rawTax (currentBank * 0.10) 
                    
                  in (Capital safeTax, Capital (currentBank - safeTax))

              newBio = (hypBiology agent) { bioBank = newBioBank }
              taxEntry = if drainAmount > 0 then [(pid, drainAmount)] else []
            in
              (agent {hypBiology = newBio }, taxEntry)