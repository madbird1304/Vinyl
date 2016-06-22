{-# OPTIONS_GHC -O2 #-}
module Main where

import Plotter
import Settings
import Help

import System.Environment
import Data.Char (toUpper)



defaultWavePath, defaultBMPPath, defaultSize, defaultDuration :: String

[defaultWavePath , defaultBMPPath , defaultSize , defaultDuration , defaultConfigPath] = ["input.wav" , "output.bmp" , "2048" , "0" , "Vinyl.conf"]

data CONTROL = S | I | D | O | C deriving (Eq,Show,Read)

argParser (a : r : gs) defl = ret ++ argParser gs defl where
    ret = case a of
        '-' : a' -> [((read $ map toUpper a') :: CONTROL , r)] 
        _ -> []
argParser _ defl = defl

defaultArgs = [(S,defaultSize) , (I,defaultWavePath) , (D,defaultDuration) , (O,defaultBMPPath) , (C,defaultConfigPath)]




main = do
    args <- getArgs
    if "--help" `elem` args then printHelpAndDie else return ()
    let Just [sz',inp,dur',outp,conf] = sequence $ map (flip lookup $ argParser args defaultArgs) [S,I,D,O,C]
    let sz = read sz'
        dur = read dur'
    settings' <- loadConfig conf
    let settings = if dur /= 0 then (Duration,dur) : settings' else settings'
    plotter <- buildPlotter settings inp
    printFigure  outp sz $ calcPoints sz plotter
