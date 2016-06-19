--{-# OPTIONS_GHC -O2 #-}
module Settings where
import System.Directory (doesFileExist)


data Setting = Period | Amplitude | Duration deriving (Eq,Show,Read)


replace _ _ "" = ""
replace c r (s:tr) | c == s = r ++ replace c r tr
                   | otherwise = s : replace c r tr


dConfPath = "Vinyl.conf"

calcDuration p a = 0.5 * p / a - p 

defaultConfig = zip [Period,Duration,Amplitude] [0.25 , calcDuration 0.25 0.005 , 0.005] 

writeDefaultConfig :: IO ()
writeDefaultConfig = do
    let period = 0.25
        amplitude = 0.005
        duration = calcDuration period amplitude
    let conf = zip [Period,Duration,Amplitude] [period,duration,amplitude]
    let str = unlines $ map (\(l,r) -> show l ++ "=" ++ show r) conf
    writeFile dConfPath str

type Settings = [(Setting,Double)]

loadConfig :: String -> IO Settings
loadConfig path = do
    fe <- doesFileExist path
    case fe of
        False -> do
            putStrLn $ path ++ " not found. Using default configuration."
            case (path == dConfPath) of 
                True -> do
                    writeDefaultConfig
                    return defaultConfig
                False -> do
                    return defaultConfig
        True -> do
            str <- readFile path
            let addBrackets sth = '(' : (sth ++ ")")
            let sets = ((map (read . addBrackets . replace '=' ",") (lines str)) :: Settings)
            let Just [a,p] = sequence $ map (flip lookup (sets ++ defaultConfig)) [Amplitude,Period]
            let d = calcDuration p a
            return $ sets ++ [(Duration,d)] ++ defaultConfig
