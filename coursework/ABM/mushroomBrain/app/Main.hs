{-# LANGUAGE OverloadedStrings #-}
module Main where

import MycelialState
import Simulation.Loop (genesisState, stepSimulation)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode)
import Data.List.Split (splitOn) -- Requires 'split' package, or use simple replacement
import System.IO (getContents)
import Data.List (foldl')
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

-- Helper to split string by comma if split package missing
splitComma :: String -> [String]
splitComma s = case break (==',') s of
    (w, "") -> [w]
    (w, _:rest) -> w : splitComma rest

main :: IO ()
main = do
    -- 1. READ INPUT (Comma Separated String)
    inputRaw <- getContents
    
    -- Filter out newlines/spaces and parse
    let cleanInput = filter (\c -> c /= '\n' && c /= ' ') inputRaw
    let stringValues = splitComma cleanInput
    let prices = catMaybes $ map readMaybe stringValues :: [Double]
    
    if null prices 
        then putStrLn "[]" -- Empty output if no input
        else do
            -- 2. CONFIG
            let config = SimConfig
                    { cfgNumAgents = 500
                    , cfgMaxLag    = 50  -- We look for patterns up to 50 ticks back
                    , cfgMaxStep   = 1.5 -- Agents crawl slowly
                    , cfgSigma     = 2.0 -- Tolerance for similarity
                    }
            
            -- 3. INITIALIZE
            let s0 = genesisState config 42
            
            -- 4. RUN LOOP (Feed the series one by one)
            let finalState = foldl' (\st p -> stepSimulation config p st) s0 prices
            
            -- 5. FORMAT OUTPUT (Sparse Matrix)
            -- We group agents into integer cells and output biomass
            let agents = sysAgents finalState
            let cells = map (\a -> MatrixCell 
                                    { x = round (fst $ hypLoc a)
                                    , y = round (snd $ hypLoc a)
                                    , b = let (Capital m) = hypBiomass a in m
                                    }
                            ) agents
            
            -- Filter out dead agents (0 biomass) to keep it clean
            let activeCells = filter (\c -> b c > 0) cells
            
            B.putStr (encode activeCells)
