module Main where

import MycelialState
import MycelialSimulation hiding (main)
import MycelialStrategy (interpretStrategy, TradingStrategy(..))
import Control.Monad.State
import Text.Printf (printf)
import System.IO (hFlush, stdout, writeFile)
import Text.Read (readMaybe)
import Data.List (sortBy, intercalate)
import Data.Ord (comparing)

-- ==========================================
-- 1. DATA GENERATORS
-- ==========================================

addNoise :: Int -> Double -> Double
addNoise t magnitude = magnitude * sin (fromIntegral t / 5.0)

generateBullRun :: Int -> [Price]
generateBullRun steps = 
    [Price (100.0 + (100.0 * fromIntegral t / fromIntegral steps) + addNoise t 2.0) | t <- [0..steps]]

generateBearMarket :: Int -> [Price]
generateBearMarket steps = 
    [Price (100.0 - (50.0 * fromIntegral t / fromIntegral steps) + addNoise t 2.0) | t <- [0..steps]]

generateVolatility :: Int -> [Price]
generateVolatility steps = 
    [Price (100.0 + 10.0 * sin (fromIntegral t / 10.0)) | t <- [0..steps]]

-- ==========================================
-- 2. JSON EXPORTER (BUNDLED HTML)
-- ==========================================

formatStateJSON :: Int -> SystemState -> String
formatStateJSON tick state =
    let
        (Price p) = mktPrice (sysEnv state)
        
        agents = sysHyphae state
        agentStrs = map (\a -> 
            let 
                loc = hypLocation a
                x = if not (null loc) then head loc else 0
                y = if length loc > 1 then loc !! 1 else 0
                pathVecs = reverse (hypPath a)
                pathJson = "[" ++ intercalate "," (map (\v -> 
                    let px = if not (null v) then head v else 0
                        py = if length v > 1 then v !! 1 else 0
                    in printf "{\"x\":%.4f,\"y\":%.4f}" px py
                    ) pathVecs) ++ "]"
            in 
                printf "{\"x\":%.4f,\"y\":%.4f,\"path\":%s}" x y pathJson :: String
            ) agents
        
        mushrooms = sysMushrooms state
        mushStrs = map (\m ->
            let loc = mushLocation m
                x = if not (null loc) then head loc else 0
                y = if length loc > 1 then loc !! 1 else 0
                (Capital mass) = mushMass m
            in printf "{\"x\":%.4f,\"y\":%.4f,\"mass\":%.2f}" x y mass :: String
            ) mushrooms

        agentJson = "[" ++ intercalate "," agentStrs ++ "]"
        mushJson = "[" ++ intercalate "," mushStrs ++ "]"
    in
        printf "{\"t\":%d,\"p\":%.2f,\"h\":%s,\"m\":%s}" tick p agentJson mushJson

runAndExport :: String -> [Price] -> IO ()
runAndExport name prices = do
    putStr $ "Running " ++ name ++ " and generating bundled HTML... "
    hFlush stdout
    
    let allStates = scanl (\s p -> execState (stepSimulation p) s) genesisState prices
    let count = length allStates
    
    if count > 0 then do
        let jsonLines = zipWith formatStateJSON [0..] allStates
        let jsData = "const simulationData = [\n" ++ intercalate ",\n" jsonLines ++ "\n];"
        
        let htmlContent = unlines 
                [ "<!DOCTYPE html>"
                , "<html><head><title>Mycelial Vis - " ++ name ++ "</title>"
                , "<style>"
                , "body { background: #111; color: #eee; font-family: sans-serif; display: flex; flex-direction: column; align-items: center; }"
                , "canvas { background: #000; border: 1px solid #333; margin: 20px; box-shadow: 0 0 20px rgba(0,255,100,0.1); }"
                , "#controls { margin-bottom: 10px; }"
                , ".stats { font-family: monospace; color: #0f0; margin-top: 5px; }"
                , ".legend { font-size: 12px; color: #888; margin-top: 5px;}"
                , "</style>"
                , "</head><body>"
                , "<h1>Mycelial Strategy Evolution: " ++ name ++ "</h1>"
                , "<div id='controls'>"
                , "  <button onclick='togglePlay()'>Play/Pause</button>"
                , "  <input type='range' id='scrubber' min='0' max='100' value='0' style='width: 400px;' oninput='scrub(this.value)'>"
                , "  <span id='tickLabel'>Tick: 0</span>"
                , "</div>"
                , "<canvas id='simCanvas' width='800' height='800'></canvas>"
                , "<div class='stats' id='statsDisplay'>Waiting...</div>"
                , "<div class='legend'>"
                , "  X-Axis: Buy Drop (Auto-Scaled) | Y-Axis: Take Profit (Auto-Scaled)<br>"
                , "  <span style='color:cyan'>Cyan:</span> Hyphae Path | <span style='color:gray'>Gray:</span> Poor Mushroom | <span style='color:gold'>Gold:</span> Rich Mushroom"
                , "</div>"
                , "<script>"
                , jsData
                , "</script>"
                , "<script>"
                , "const canvas = document.getElementById('simCanvas');"
                , "const ctx = canvas.getContext('2d');"
                , "const stats = document.getElementById('statsDisplay');"
                , "const scrubber = document.getElementById('scrubber');"
                , "const tickLabel = document.getElementById('tickLabel');"
                , "let currentTick = 0; let isPlaying = false; let timer = null;"
                , "const WIDTH = 800; const HEIGHT = 800;"
                , "// AUTO-SCALE LOGIC"
                , "let MAX_X = 0.10; let MAX_Y = 0.20;"
                , "function calculateBounds() {"
                , "  let maxX = 0.01; let maxY = 0.01;"
                , "  simulationData.forEach(frame => {"
                , "     frame.h.forEach(a => { if(a.x > maxX) maxX=a.x; if(a.y > maxY) maxY=a.y; });"
                , "     frame.m.forEach(m => { if(m.x > maxX) maxX=m.x; if(m.y > maxY) maxY=m.y; });"
                , "  });"
                , "  // Add 10% padding"
                , "  MAX_X = maxX * 1.1;"
                , "  MAX_Y = maxY * 1.1;"
                , "  console.log('Auto-Scale Bounds:', MAX_X, MAX_Y);"
                , "}"
                , "function toScreen(x, y) { return { x: (x / MAX_X) * WIDTH, y: HEIGHT - ((y / MAX_Y) * HEIGHT) }; }"
                , "function drawGrid() {"
                , "  ctx.strokeStyle = '#222'; ctx.lineWidth = 1; ctx.textAlign='center';"
                , "  for(let i=0; i<=MAX_X; i+=MAX_X/10) { let p=toScreen(i,0).x; ctx.beginPath(); ctx.moveTo(p,0); ctx.lineTo(p,HEIGHT); ctx.stroke(); ctx.fillStyle='#555'; ctx.fillText((i*100).toFixed(1)+'%', p, HEIGHT-5); }"
                , "  for(let i=0; i<=MAX_Y; i+=MAX_Y/10) { let p=toScreen(0,i).y; ctx.beginPath(); ctx.moveTo(0,p); ctx.lineTo(WIDTH,p); ctx.stroke(); ctx.fillStyle='#555'; ctx.fillText((i*100).toFixed(1)+'%', 15, p-5);}"
                , "}"
                , "function lerpColor(r1, g1, b1, r2, g2, b2, t) {"
                , "    t = Math.min(1, Math.max(0, t));"
                , "    return `rgb(${Math.round(r1 + (r2-r1)*t)}, ${Math.round(g1 + (g2-g1)*t)}, ${Math.round(b1 + (b2-b1)*t)})`;"
                , "}"
                , "function drawFrame(idx) {"
                , "  if(!simulationData) return;"
                , "  const frame = simulationData[idx];"
                , "  ctx.fillStyle='black'; ctx.fillRect(0,0,WIDTH,HEIGHT);"
                , "  drawGrid();"
                , "  ctx.strokeStyle='cyan'; ctx.lineWidth=1;"
                , "  frame.h.forEach(a => {"
                , "     if (a.path && a.path.length > 0) {"
                , "         ctx.beginPath();"
                , "         let start = toScreen(a.path[0].x, a.path[0].y);"
                , "         ctx.moveTo(start.x, start.y);"
                , "         for(let i=1; i<a.path.length; i++) {"
                , "             let p = toScreen(a.path[i].x, a.path[i].y);"
                , "             ctx.lineTo(p.x, p.y);"
                , "         }"
                , "         ctx.stroke();"
                , "     }"
                , "     let head = toScreen(a.x, a.y);"
                , "     ctx.fillStyle='cyan'; ctx.beginPath(); ctx.arc(head.x,head.y,3,0,Math.PI*2); ctx.fill();"
                , "  });"
                , "  frame.m.forEach(m => {"
                , "    let p=toScreen(m.x,m.y);"
                , "    let intensity = Math.min(m.mass / 5000, 1);"
                , "    ctx.fillStyle = lerpColor(128,128,128, 255,215,0, intensity);"
                , "    let radius = 5 + (m.mass / 500);"
                , "    ctx.beginPath(); ctx.arc(p.x,p.y, radius, 0, Math.PI*2); ctx.fill(); ctx.strokeStyle='#fff'; ctx.stroke();"
                , "  });"
                , "  stats.innerText = `Tick: ${frame.t} | Price: $${frame.p.toFixed(2)} | Agents: ${frame.h.length} | Mushrooms: ${frame.m.length}`;"
                , "  scrubber.value = idx; tickLabel.innerText = 'Tick: '+idx;"
                , "}"
                , "function play() { if(currentTick < simulationData.length-1) { currentTick++; drawFrame(currentTick); timer=requestAnimationFrame(play); } else isPlaying=false; }"
                , "function togglePlay() { if(isPlaying){ cancelAnimationFrame(timer); isPlaying=false; } else { isPlaying=true; play(); } }"
                , "function scrub(val) { currentTick=parseInt(val); drawFrame(currentTick); }"
                , "window.onload = function() { calculateBounds(); scrubber.max=simulationData.length-1; drawFrame(0); }"
                , "</script>"
                , "</body></html>"
                ]
        
        writeFile "viewer_bundled.html" htmlContent
        putStrLn "Done!"
        putStrLn ">>> Success! Open 'viewer_bundled.html'."
    else
        putStrLn "Error: Simulation produced no states."

-- ==========================================
-- 3. REPORTING HELPERS
-- ==========================================

calculateTVL :: SystemState -> Double
calculateTVL state =
    let 
        (GlobalWallet (Capital w)) = sysWallet state
        (Price p) = mktPrice (sysEnv state)
        agentVal = sum $ map (\a -> 
            let (Quantity q) = posQuantity (hypHoldings a)
            in q * p + (case bioBank (hypBiology a) of Capital c -> c)
            ) (sysHyphae state)
        mushVal = sum $ map (\m -> case mushMass m of Capital c -> c) (sysMushrooms state)
        sporeVal = sum $ map (\s -> case sporeCapital s of Capital c -> c) (sysSpores state)
    in
        w + agentVal + mushVal + sporeVal

printReport :: String -> SystemState -> IO ()
printReport label state = do
    let tvl = calculateTVL state
    let hCount = length (sysHyphae state)
    let mCount = length (sysMushrooms state)
    let sCount = length (sysSpores state)
    let (GlobalWallet (Capital w)) = sysWallet state
    let (Price p) = mktPrice (sysEnv state)
    
    printf "\n--- %s REPORT ---\n" label
    printf "Market Price : %8.2f\n" p
    printf "Total Value  : %8.2f (TVL)\n" tvl
    printf "Global Wallet: %8.2f\n" w
    printf "Population   : %d Hyphae | %d Mushrooms | %d Spores\n" hCount mCount sCount

printMushroomDetails :: SystemState -> IO ()
printMushroomDetails state = do
    case sysMushrooms state of
        [] -> putStrLn "No Mushrooms formed."
        ms -> do
            putStrLn "\n--- MUSHROOM DETAILS (Successful Strategies) ---"
            let sortedMs = sortBy (flip $ comparing (\m -> case mushMass m of Capital c -> c)) ms
            mapM_ (\m -> do
                let (Capital mass) = mushMass m
                let loc = mushLocation m
                let strat = interpretStrategy loc
                let dropPct = stratDropThreshold strat * 100.0
                let profitPct = stratProfitTarget strat * 100.0
                printf "ID: %3d | Mass: %8.2f | Buy Drop: %5.2f%% | Take Profit: %5.2f%%\n" 
                       (mushId m) mass dropPct profitPct
                ) sortedMs

printTopAgents :: SystemState -> IO ()
printTopAgents state = do
    let agents = sysHyphae state
    case agents of
        [] -> return ()
        as -> do
            putStrLn "\n--- TOP 5 ACTIVE AGENTS ---"
            let richAgents = take 5 $ sortBy (flip $ comparing (\a -> case bioBank (hypBiology a) of Capital c -> c)) as
            mapM_ (\a -> do
                let (Capital bank) = bioBank (hypBiology a)
                let loc = hypLocation a
                let strat = interpretStrategy loc
                let dropPct = stratDropThreshold strat * 100.0
                let profitPct = stratProfitTarget strat * 100.0
                printf "ID: %3d | Bank: %8.2f | Buy Drop: %5.2f%% | Take Profit: %5.2f%%\n" 
                       (hypId a) bank dropPct profitPct
                ) richAgents

-- ==========================================
-- 4. INTERACTIVE RUNNER
-- ==========================================

runSimulation :: String -> [Price] -> IO ()
runSimulation name prices = do
    putStrLn $ "\nInitializing " ++ name ++ "..."
    let simulation = mapM_ stepSimulation prices
    let finalState = execState simulation genesisState
    printReport "FINAL" finalState
    printMushroomDetails finalState
    printTopAgents finalState

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

main :: IO ()
main = do
    putStrLn "\n=========================================="
    putStrLn "   MYCELIAL TRADING HIVE - SIMULATOR v1   "
    putStrLn "=========================================="
    putStrLn "1. Bull Run (Noisy Uptrend)"
    putStrLn "2. Crypto Winter (Noisy Downtrend)"
    putStrLn "3. Volatility Chop (Sine Wave)"
    putStrLn "4. Custom (Stub)"
    putStrLn "5. Generate Visualization (Web)"
    putStrLn "q. Quit"
    
    choice <- prompt "\nSelect Scenario: "
    
    case choice of
        "q" -> putStrLn "Exiting."
        "1" -> setupRun "Bull Run" generateBullRun False
        "2" -> setupRun "Bear Market" generateBearMarket False
        "3" -> setupRun "Volatility" generateVolatility False
        "5" -> do
             putStrLn "Which scenario to visualize?"
             putStrLn "1. Bull / 2. Bear / 3. Volatility"
             scen <- prompt "> "
             case scen of
                 "1" -> setupRun "Bull Run" generateBullRun True
                 "2" -> setupRun "Bear Market" generateBearMarket True
                 "3" -> setupRun "Volatility" generateVolatility True
                 _   -> do
                     putStrLn "Invalid scenario selection! Returning to menu."
                     main
        _   -> do 
            putStrLn "Invalid selection."
            main

setupRun :: String -> (Int -> [Price]) -> Bool -> IO ()
setupRun name generator isExport = do
    durStr <- prompt "Enter Duration (ticks, default 500): "
    let duration = case readMaybe durStr of
            Just n  -> n
            Nothing -> 500
            
    if isExport 
        then runAndExport name (generator duration)
        else runSimulation name (generator duration)
    
    putStrLn "\nDone."
    main