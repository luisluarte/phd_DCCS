module Simulation.Macro where

import MycelialState
import MycelialPhysics (euclideanDistance, calculatePressure)
import Data.List (foldl')


-- global definition simplifies the computation a lot
-- with this is there's basically only 1 field
sensingRadius :: Double
sensingRadius = 0.20

-- this is the computation of the scalar Phi(x) at a given location
-- sum pressure from all nearby agents weighted by a gaussian kernel
calculateLocalField :: ParamVector -> [HyphalTip] -> [MushroomBody] -> Price -> Double
calculateLocalField loc agents mushrooms currentPrice =
  let
    sigma = sensingRadius
    kernel r = exp (-(r**2) / (2 * sigma**2))
    
    -- 1. Agent Contributions (Worker Pheromones)
    agentContribs = map (\a -> 
      let 
        dist = euclideanDistance loc (hypLocation a)
        pressure = calculatePressure currentPrice a
      in if dist < (sensingRadius * 5.0) 
        then pressure * kernel dist
        else 0
        ) agents

    -- Mushroom Contributions (Mother Pheromones)
    -- Mushrooms emit pressure proportional to their Mass (Capital)
    -- this fixes the issue of spores never germinating
    -- initial 'good' mushroom provide fertile ground for germination
    mushContribs = map (\m -> 
      let
          dist = euclideanDistance loc (mushLocation m)
          (Capital mass) = mushMass m
          -- Heuristic: Pressure = Mass / 10. A 2500 mass mushroom emits 250 pressure.
          pressure = mass / 10.0  -- hardcoded but works
      in if dist < (sensingRadius * 5.0)
         then pressure * kernel dist
         else 0
      ) mushrooms

  in
    sum agentContribs + sum mushContribs

type TaxMap = [(MushroomId, Capital)]

applyDrain :: HyphalTip -> [MushroomBody] -> Price -> [HyphalTip] -> (HyphalTip, TaxMap)
applyDrain agent mushrooms currentPrice allAgents =
  let
    pid = hypParentId agent
    -- note here that filter walks through mushroom list 
    -- calling \m to return bool TRUE/FALSE, and keeps
    -- only the elements that are TRUE
    parentMaybe = filter (\m -> mushId m == pid) mushrooms
  in
    case parentMaybe of
      [] -> (agent, [])
      (parent:_) ->
        let
          -- this is the pressure at hyphal tip
          psi_i = calculatePressure currentPrice agent
          
          -- Updated: Pass mushrooms to field calc
          -- (mushLocation parent) is the location in the parameter space 
          -- of the parent mushroom
          -- this is computed at the mushroom parameter space
          phi_m = calculateLocalField (mushLocation parent) allAgents mushrooms currentPrice

          k_vac = geneVacuumCoefficient (mushGenome parent)
          vacuum = -(k_vac * phi_m)

          (Capital currentBank) = bioBank (hypBiology agent)
          -- if agent_i cannot provide capital to the parent mushroom
          -- its deemed as toxic and its going to die :c
          isToxic = psi_i < vacuum

          -- this to compute, after death, how much bank should be returned
          (drainAmount, newBioBank) = if isToxic
            then (Capital currentBank, Capital 0)
            else
              let
                -- this is to compute the actual tax
                -- age is a 'modulator' of tax
                tau = fromIntegral (bioAge (hypBiology agent)) :: Double
                flux = tau * (psi_i - vacuum)
                rawTax = max 0.0 flux
                -- to deal when full taxation is not possible
                cappedTax = min rawTax currentBank
              in (Capital cappedTax, Capital (currentBank - cappedTax))

          newBio = (hypBiology agent) { bioBank = newBioBank }
          taxEntry = if drainAmount > 0 then [(pid, drainAmount)] else []
        in
          -- how much does agent_i has to pay in taxes
          (agent {hypBiology = newBio }, taxEntry)