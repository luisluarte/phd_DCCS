module Simulation.Macro where

import MycelialState
import MycelialPhysics (euclideanDistance, calculatePressure)
import Data.List (foldl')
import qualified Data.Map.Strict as Map -- Added Map import

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

-- CACHED MAP TYPE: (MushroomBody, CachedFieldStrength)
type MushroomCache = Map.Map MushroomId (MushroomBody, Double)

-- UPDATED: Now takes the Cache Map instead of raw lists
applyDrain :: HyphalTip -> MushroomCache -> Price -> (HyphalTip, TaxMap)
applyDrain agent mushCache currentPrice =
  let
    pid = hypParentId agent
    parentData = Map.lookup pid mushCache -- O(log M) lookup!
  in
    case parentData of
      Nothing -> (agent, [])
      Just (parent, phi_m) -> -- We get the pre-calculated phi_m here!
        let
          psi_i = calculatePressure currentPrice agent
          
          -- NO MORE EXPENSIVE CALCULATION HERE
          
          k_vac = geneVacuumCoefficient (mushGenome parent)
          vacuum = -(k_vac * phi_m)

          (Capital currentBank) = bioBank (hypBiology agent)
          isToxic = psi_i < vacuum

          (drainAmount, newBioBank) = if isToxic
            then (Capital currentBank, Capital 0)
            else
              let
                tau = fromIntegral (bioAge (hypBiology agent)) :: Double
                flux = tau * (psi_i - vacuum)
                rawTax = max 0.0 flux
                cappedTax = min rawTax currentBank
              in (Capital cappedTax, Capital (currentBank - cappedTax))

          newBio = (hypBiology agent) { bioBank = newBioBank }
          taxEntry = if drainAmount > 0 then [(pid, drainAmount)] else []
        in
          (agent {hypBiology = newBio }, taxEntry)