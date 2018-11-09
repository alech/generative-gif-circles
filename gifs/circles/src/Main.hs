{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Main where

import Diagrams.Prelude
import Diagrams.TwoD.Size (mkSizeSpec2D)
import Data.Colour.SRGB (sRGB)
import Diagrams.Backend.Rasterific

coloredCircles i =
    position circles # lw thin # bgFrame 0.1 white
    where
        circles = (\x -> (p2 (cos $ x/10, sin $ x/10),
                          circle 1 # lc (color x i))) <$> [0..421]
        color x i =
            sRGB (cos $ (x+300)/10+4) (sin $ (x-50-i)/10) (cos $ x/10)

main :: IO ()
main = 
    animatedGif "circles.gif"
                (mkSizeSpec2D (Just 400) (Just 400))
                LoopingForever
                1
                (coloredCircles <$> ([0..63] ++ reverse [1..62]))
