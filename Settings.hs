--{-# OPTIONS_GHC -O2 #-}
module Settings where
import System.Directory (doesFileExist)

data SET = T | AMP | DURATION deriving (Eq,Show,Read)

type Settings = [(SET,Double)]

defaultSettings :: Settings
defaultSettings = [
    (T,0.25),
    (AMP,0.005)
    --(DURATION, 90.0)
    ]

check :: Settings -> Maybe Settings
check settings = do
    t <- lookup T   settings
    a <- lookup AMP settings
    case lookup DURATION settings of
        Just sth -> return settings
        _ -> return $ (DURATION,(1.0 - 2*a) * t / (2*a)) : settings

initialize :: String -> IO Settings
initialize path = do
    fe <- doesFileExist path
    str' <- if fe then readFile path else return "[]"
    let str = case str' of
            '#':_ -> "[]"
            _ -> str'
    case (check ((read str) :: Settings)) of
        Just sth -> do
            print sth
            return sth
        _ -> do
            let Just ret = check defaultSettings
            return ret


