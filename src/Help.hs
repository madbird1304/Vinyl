module Help where

import System.Exit (exitSuccess)

message :: String
message = unlines ["Hello! I'm Vinyl.",
                   "Just a program that doing stupid things.",
                   "Let me buld old-good vinyl track (.bmp) from any your sound file (.wav).",
                   "Try me with next params:",
                   "\t--help = show this message",
                   "\t-i arg = use arg as input sound file (*.wav only, default - input.wav)",
                   "\t-o arg = use arg as output bitmap file (*.bmp only, default - output.bmp)",
                   "\t-d n = take first n seconds from input file (floatings is available)",
                   "\t-s n = set bitmap size to n*n (default n = 2048)",
                   "\t-c arg = use arg as config file"]


printHelpAndDie :: IO ()
printHelpAndDie = do
    putStrLn message
    exitSuccess