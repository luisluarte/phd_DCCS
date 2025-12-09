module SP500Data where

import MycelialState (Price(..))
import System.IO (readFile, hPutStrLn, stderr)
import Text.Read (readMaybe)
import Control.Exception (catch, IOException)

-- ==========================================
-- CSV PARSER (Standard Lib Only)
-- ==========================================

-- Helper to split strings by comma
splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]] 
  where 
    f c l@(x:xs) | c == delimiter = []:l
                 | otherwise = (c:x):xs
    f _ [] = [[]]

-- Parse a single CSV line from SPX.csv
-- Format: Date,Open,High,Low,Close,Adj Close,Volume
-- Example: 1927-12-30,17.66,17.66,17.66,17.66,17.66,0
parseLine :: String -> Maybe (Int, Double)
parseLine line = 
    let columns = splitBy ',' line
    in if length columns >= 6 
       then 
         let 
             dateStr = columns !! 0 -- "YYYY-MM-DD"
             yearPart = take 4 dateStr
             pricePart = columns !! 5 -- Adj Close is index 5
         in 
             case (readMaybe yearPart, readMaybe pricePart) of
                 (Just y, Just p) -> Just (y, p)
                 _ -> Nothing
       else Nothing

-- ==========================================
-- IO LOADER
-- ==========================================

loadSP500Data :: IO [Price]
loadSP500Data = catch (do
    putStrLn "Reading 'SPX.csv'..."
    content <- readFile "SPX.csv"
    let linesOfFile = lines content
    
    -- Drop Header and Parse
    let parsedRows = map parseLine (drop 1 linesOfFile)
    
    -- Filter for Years >= 2000 and Extract Price
    let prices = [Price p | Just (y, p) <- parsedRows, y >= 2018]
            
    -- Sanity Check
    if null prices
        then do
            hPutStrLn stderr "Warning: 'SPX.csv' was read but no data found for >= 1930."
            return []
        else do
            putStrLn $ "Successfully loaded " ++ show (length prices) ++ " trading days (1930-2020)."
            return prices
    ) handler
  where
    handler :: IOException -> IO [Price]
    handler _ = do
        hPutStrLn stderr "ERROR: Could not find 'SPX.csv'."
        hPutStrLn stderr "Please ensure the file is in the root directory."
        return []