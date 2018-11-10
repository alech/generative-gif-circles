{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Main where

import Diagrams.Prelude
import Diagrams.TwoD.Size (mkSizeSpec2D)
import Data.Colour.SRGB (sRGB)
import Diagrams.Backend.Rasterific

coloredCircles i =
    (position circles # lw thin) `atop` (circle 2 # lc white) # bgFrame 0.1 white
    where
        colors = 
            [sRGB 0.02 0.27 0.58,
             sRGB 0.00 0.07 0.29,
             sRGB 0.84 0.00 0.04]
        circles = [(p2 (cos(fromIntegral x/10), sin(fromIntegral x/10)),
                    circle 1 # lc (colors !! (x `mod` 3)))  | x <- [0..i]]

main :: IO ()
main = 
    animatedGif "circles2.gif"
                (mkSizeSpec2D (Just 560.0 :: Maybe Double)
                              (Just 560.0 :: Maybe Double))
                LoopingForever
                20
                (coloredCircles <$> [62,62,62] ++ [62,126..15*62] ++ reverse [62,126..15*62])
