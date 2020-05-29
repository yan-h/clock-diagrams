module Draw where

import Music.PC
import Diagrams.Prelude hiding (trace)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Path.Boolean as B
import Data.Colour.CIE
import Data.Colour.CIE.Illuminant
import Debug.Trace

aCol, abCol, bCol, neutral :: (Ord a, Floating a) => Colour a
aCol = labColor  (1.5/16)  1    0.8 
abCol = labColor (-1.5/16) 0.8  0.5
bCol = labColor  (-4.5/16) 0.6  0.2
neutral = labColor 0 0 0.85

labColor :: (Ord a, Floating a) => a -> a -> a -> Colour a
labColor hue saturation lightness = 
  cieLAB d65 (lightness * 100) (saturation * 128 * cos (tau * hue)) (saturation * 128 * sin (tau * hue))

semis :: Int -> Double
semis x = fromIntegral x * (-1/12)

clock :: (a -> Int -> Diagram B) -> a -> Diagram B
clock fn x = 
  let go :: Int -> Diagram B -> Diagram B
      go idx acc = 
        let pcNode = fn x idx
         in acc `atop` pcNode # rotate (semis idx @@ turn)
   in foldr go mempty [0..11] # pad 1.1

baseClock :: Diagram B
baseClock = clock greyCircle undefined
  where greyCircle :: a -> Int -> Diagram B
        greyCircle = const . const $ circle (tau / 12) 
                        # fc lightgray 
                        # lcA transparent 
                        # translate (r2 (0, 1))

scaleClock :: Scale -> Diagram B
scaleClock = clock scaleCircle 
  where 
    scaleCircle :: Scale -> Int -> Diagram B
    scaleCircle scale idx = 
      if scale `contains` pc idx then 
        strokeLine (lineFromVertices [origin, p2 (0, 1)]) # lc red # lw strokeWidth # lineCap LineCapRound
          <> circle (tau / 36) # fc pink # lcA transparent # translate (r2 (0, 1)) 
      else 
        circle (tau / 36) # fc lightgray # lcA transparent # translate (r2 (0, 1))
      where strokeWidth = if root scale == pc idx then 16 else 10

twoScaleClock :: Scale -> Scale -> Diagram B
twoScaleClock scaleA scaleB = 
  let go :: (Int -> Diagram B) -> Int -> Diagram B -> Diagram B
      go fn idx acc = acc `atop` fn idx # rotate (semis idx @@ turn)

      pcWedge :: Int -> Diagram B
      pcWedge idx = 
          wedge 0.5 (rotate (-1/24 @@ turn) yDir) (1/12 @@ turn) # fc fillCol # lcA transparent
          where fillCol
                  | scaleA `contains` pc idx && scaleB `contains` pc idx = abCol
                  | scaleA `contains` pc idx = aCol
                  | scaleB `contains` pc idx = bCol
                  | otherwise = neutral

      intervalTicksPath = foldr go (square 2) [0..5]
        where go :: Double -> Path V2 Double -> Path V2 Double
              go x acc = 
                let gridline = rect 0.03 2 # rotate (x * 1/12 @@ turn)
                in  B.difference Winding acc gridline
      pcTicksPath = intervalTicksPath # rotate (1/24 @@ turn)
  in foldr (go pcWedge) mempty [0..11] # clipBy pcTicksPath

inversePair :: Scale -> Scale -> Diagram B
inversePair scaleA scaleB = 
  let rootDiff = (root scaleB) -| (root scaleA)
      scaleB' = transpose (-rootDiff) scaleA
      scaleA' = transpose (-rootDiff) scaleB
  in  if scaleA == scaleA' && scaleB' == scaleB
        then labeledTrans scaleA scaleB
      else vsep 0.2 [
        labeledTrans scaleA scaleB
      , labeledTrans scaleA' scaleB'
      ]

labeledTrans :: Scale -> Scale -> Diagram B
labeledTrans scaleA scaleB = vsep 0.16
  [ twoScaleClock scaleA scaleB 
  , text str # fontSize 24]
  where str = show (root scaleB - root scaleA) ++ itvsString (offsets scaleA) ++ itvsString (offsets scaleB)

majorMinorTransitions :: Diagram B
majorMinorTransitions =
  vsep 0.6 [
      hsep 0.6 [ 
          hsep 0.08 # composeAligned centerY $ 
            [ inversePair (majorTriad pc0) (transpose x (majorTriad pc0))
            | x <- [up1 .. up6]]
        , hsep 0.08 # composeAligned centerY $ 
            [ inversePair (minorTriad pc0) (transpose x (minorTriad pc0))
            | x <- [up1 .. up6]]
      ]
      , hsep 0.08 [ inversePair (majorTriad pc0) (transpose x (minorTriad pc0))
                | x <- [up0 .. upE]]
    ] # bgFrame 0.1 white
