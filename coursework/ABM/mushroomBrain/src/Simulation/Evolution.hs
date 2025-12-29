module Simulation.Evolution 
    ( mutateFloat
    , mutateGenome
    , randomizeGenome -- Explicitly exported
    ) where

import MycelialState
import System.Random (StdGen, randomR, mkStdGen)


-- implementation of levy-flight distribution
-- we will approximate a levy flight step as
-- delta_x = u / |v|^1/alpha, where u and v are standard normal dist.
-- in foraging the dist. is usually specified with alpha = 1.5

-- first box muller for normal dist.
boxMuller :: StdGen -> (Double, StdGen)
boxMuller rng =
	let
		(u1, rng1) = randomR (0.0, 1.0) rng
		(u2, rng2) = randomR (0.0, 1.0) rng1
		r = sqrt (-2.0 * log u1)
		theta = 2.0 * pi * u2
	in
		(r * cos theta, rng2)

-- levy flight approximation
levyFlight :: Double -> Double -> StdGen -> (Double, StdGen)
levyFlight alpha scale rng =
	let
		(u, rng1) = boxMuller rng
		(v, rng2) = boxMuller rng1

		step = u / (abs v ** (1.0 / alpha))
	in
		(step * scale, rng2)

mutateFloat :: Double -> Double -> StdGen -> (Double, StdGen)
mutateFloat val scale rng =
    let
    	(noise, newRng) = levyFlight 1.5 scale rng -- 1.5 is the typical value in foraging
        newValue = val + noise
    in
    	(max 0.0001 newValue, newRng) -- clamped

mutateGenome :: Genome -> StdGen -> Genome
mutateGenome g rng =
    let
        (r1, rng1) = mutateFloat (geneGreed g) 0.05 rng
        (r2, rng2) = mutateFloat (geneTurbulence g) 1.0 rng1
        (r3, rng3) = mutateFloat (geneGrowthRate g) 0.01 rng2
        (r4, rng4) = mutateFloat (geneBaseOrder g) 2.0 rng3
        (r5, rng5) = mutateFloat (genePhiCritical g) 0.5 rng4
        (r6, rng6) = mutateFloat (geneReproductiveInvest g) 0.05 rng5
        (r7, rng7) = mutateFloat (geneVacuumCoefficient g) 0.1 rng6
        (r8, _)    = mutateFloat (geneDevMult g) 0.05 rng7
    in
        g { geneGreed = min 0.99 r1
          , geneTurbulence = r2
          , geneGrowthRate = r3
          , geneBaseOrder = r4
          , genePhiCritical = r5
          , geneReproductiveInvest = min 0.9 (max 0.1 r6)
          , geneVacuumCoefficient = min 1.0 (max 0.01 r7)
          , geneDevMult = r8
          }


randomizeGenome :: Genome -> Int -> Genome
randomizeGenome template seed = mutateGenome template (mkStdGen seed)
