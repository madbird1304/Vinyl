module Settings where

import System.Directory (doesFileExist)

data Setting = Period | Amplitude | Duration deriving (Eq,Show,Read)

type Settings = [(Setting,Double)]

replace _ _ "" = ""
replace c r (s:tr) | c == s = r ++ replace c r tr
                   | otherwise = s : replace c r tr


dConfPath = "Vinyl.conf"

calcDuration p a = 0.5 * p / a - p 

defaultConfig = zip [Period,Duration,Amplitude] [0.25 , calcDuration 0.25 0.005 , 0.005] 

writeDefaultConfig :: IO ()
writeDefaultConfig = writeFile dConfPath (unlines $ map format defaultConfig) where
    format = \(l,r) -> show l ++ "=" ++ show r
   

loadConfig :: String -> IO Settings
loadConfig path = do
    fe <- doesFileExist path    
    case fe of
        False -> do
            putStrLn $ "File " ++ path ++ " not found. Using default configuration."
            case (path == dConfPath) of 
                True -> do
                    writeDefaultConfig
                    return defaultConfig
                False -> do
                    return defaultConfig
        True -> do
            str <- readFile path
            let addBrackets = \sth -> '(' : (sth ++ ")")
            let sets = ((map (read . addBrackets . replace '=' ",") (lines str)) :: Settings)
            let Just [a,p] = sequence $ map (flip lookup (sets ++ defaultConfig)) [Amplitude,Period]
            let d = calcDuration p a
            return $ sets ++ [(Duration,d)] ++ defaultConfig
