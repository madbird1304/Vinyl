module PlotterIO where
import Spire
import Codec.BMP
import Data.List
import qualified Data.ByteString as BS
import Data.WAVE
import Settings
import qualified Data.Vector.Unboxed as V


bgColor, lineColor :: BS.ByteString

lineColor = BS.pack [   0,   0,   0,   0]
bgColor   = BS.pack [ 255, 255, 255,   0]

------------------------------------------------------------------------------------------------------

bmpFromPoints :: Int -> [V2I] -> BMP
bmpFromPoints sz lst = packRGBA32ToBMP24 sz sz . BS.concat . fn (0,0) . map head . group . sort $ lst where
    fn (i,j) ((m,n):r) = lineColor : replicate fill bgColor ++ fn (m,n) r where 
        fill = (m-i-1)*sz + (sz-j-1 + n)
    fn (i,j) _ = lineColor : replicate fill bgColor where 
        fill = (sz-i-1)*sz + (sz-j-1)



{-# INLINE line #-}
line :: V2I -> V2I -> [V2I]
line l@(x1,y1) r@(x2,y2) | abs (x1 - x2) < abs (y1 - y2) = if y1 == y2 then [l,r] else map fn2 [min y1 y2 .. max y1 y2]
                         | otherwise = if x1 == x2 then [l,r] else map fn1 [min x1 x2 .. max x1 x2] 
    where
        fn1 = \ x -> (x , y1 + (y1 - y2) * (x1 - x) `div` (x2 -x1))
        fn2 = \ y -> (x1 + (x1 - x2) * (y1 - y) `div` (y2 -y1) , y)




calcPoints :: Int -> Plotter -> [V2I]
calcPoints sz' ((Spire tp dur fn),sound) = concat $ zipWith line bpoints (tail bpoints) where
    sz = fromIntegral sz'
    delta = tp / sz 
    npoints = [0.0,delta .. dur-delta]
    bpoints = map (vmap (floor . (/2) . (*sz)) . fn sound) npoints


printFigure :: String -> Int -> [V2I] -> IO ()
printFigure path sz = writeBMP path . bmpFromPoints sz


getWave :: String -> Spire -> IO (SoundClosure,Double)
getWave path (Spire _ dur _) = do
    WAVE (WAVEHeader _ fr _ (Just fc)) samples <- getWAVEFile path
    let fr' = fromIntegral fr
    let samples' = V.fromListN fc $ map (sampleToDouble . head) samples
    let closure = \t -> let n = floor $ t * fr' in 
            case t < dur && n < fc of
                True -> samples' V.! n
                _ -> 0.0
    return (closure, foldl1 (/) $ map fromIntegral [fc,fr])



buildPlotter :: Settings -> String -> IO (Spire,SoundClosure)
buildPlotter set path = do
    let Just [p,a,d] = sequence $ map (flip lookup set) [T,AMP,DURATION]
        spire@(Spire per dur fn) = makeSpire p a d 
    (sound,dur') <- getWave path spire
    return $ (Spire per (min dur dur') fn, sound)
