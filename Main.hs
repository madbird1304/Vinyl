--{-# OPTIONS_GHC -O2 #-}
module Vinyl where

import System.Environment
import PlotterIO
import Settings


main :: IO ()
main = do
    [sz',inp,dur',outp] <- (return . parseArgs) =<< getArgs
    let sz = read sz'
        dur = read dur'
    settings' <- initialize ""
    let settings = (if dur /= 0 then [(DURATION,dur)] else []) ++ settings'
    plotter <- buildPlotter (settings) inp
    printFigure  outp sz $ calcPoints sz plotter
    where
        defaultArgs = defaultSize : defaultWavePath : defaultDuration : defaultBMPPath : []
        parseArgs ret@(_:_:_:_:_) = ret
        parseArgs ret@(_:_:_:_) = take 3 ret ++ drop 3 defaultArgs
        parseArgs ret@(_:_:_) = take 2 ret ++ drop 2 defaultArgs
        parseArgs ret@(_:_)   = take 1 ret ++ drop 1 defaultArgs
        parseArgs _ = defaultArgs