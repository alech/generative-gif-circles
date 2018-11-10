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
            [sRGB 0.024 0.522 0.529,
             sRGB 0.067 0.184 0.255,
             sRGB 0.31 0.725 0.624]
        circles = [(p2 (cos(fromIntegral x/10), sin(fromIntegral x/10)),
                    circle 1 # lc (colors !! (x `mod` 3)))  | x <- [0..i]]

main :: IO ()
main = 
    animatedGif "circles2.gif"
                (mkSizeSpec2D (Just 400.0 :: Maybe Double)
                              (Just 400.0 :: Maybe Double))
                LoopingNever
                1
                (coloredCircles <$> [0,4..390])
