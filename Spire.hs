module Spire where

type V2 a = (a , a)

type V2D = V2 Double
type V2I = V2 Int

type SoundClosure = (Double -> Double) 
type Plotter = (Spire,SoundClosure)

data Spire = Spire Double Double ((Double -> Double) -> Double -> V2D)



{-# INLINE vmap #-}
vmap :: (a -> b) -> V2 a -> V2 b
vmap fn = \ (a,b) -> (fn a,fn b)


makeSpire :: Double -> Double-> Double -> Spire
makeSpire period ampl duration = Spire (period/2) duration $ \ a t -> let
    p = 1 - ampl - adp * t + ampl * (a t) * 0.5
    h = p2dp * t
    in vmap ((+ 1)  . (*p)) (sin h, cos h) 
    where
        p2dp = 2 * pi / period
        adp = (2*ampl) / period
    



                             
