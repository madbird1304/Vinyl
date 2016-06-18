{-# OPTIONS_GHC -O2 #-}
module Main where

import System.Environment
import PlotterIO
import Settings

defaultArgs = [defaultSize , defaultWavePath , defaultDuration , defaultBMPPath]
parseArgs ret = let n = length ret in 
    ret ++ drop n defaultArgs 

main :: IO ()
main = do
    [sz',inp,dur',outp] <- (return . parseArgs) =<< getArgs
    let sz = read sz'
        dur = read dur'
    settings' <- initialize ""
    let settings = if dur /= 0 then (DURATION,dur) : settings' else settings'
    plotter <- buildPlotter (settings) inp
    printFigure  outp sz $ calcPoints sz plotter

        
        