module Settings where

data SET = T | AMP | DURATION deriving (Eq,Show,Read)

type Settings = [(SET,Double)]

defaultSettings :: Settings
defaultSettings = [
    (T,1),
    (AMP,0.01)
    --(DURATION, 90.0)
    ]


defaultWavePath, defaultBMPPath, defaultSize, defaultDuration:: String
defaultWavePath = "input.wav"
defaultBMPPath = "output.bmp"
defaultSize = "512"
defaultDuration = "0"

check :: Settings -> Maybe Settings
check settings = do
    t <- lookup T   settings
    a <- lookup AMP settings
    let d = (1.0 - 2*a) * t / (2*a)
    return $ (DURATION,d):settings

initialize :: String -> IO Settings
initialize "" = do
    let Just ret = check defaultSettings
    return ret
initialize path = do
    str <- readFile path
    case check $ read str of
        Just sth -> return sth
        _ -> initialize "" 
    --return $ ret


