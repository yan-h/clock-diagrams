module Draw where

import Music.PC
import Diagrams.Prelude hiding (trace)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Path.Boolean as B
import Data.Colour.CIE
import Data.Colour.CIE.Illuminant
import Debug.Trace

hue :: Double -> Double
hue x = tau * x

aCol, abCol, bCol :: (Ord a, Floating a) => Colour a
aCol = labColor  (1.5/16)  1    0.8 
abCol = labColor (-1.5/16) 0.8  0.5
bCol = labColor  (-4.5/16) 0.6  0.2
aCol' = labColor (1.5/16) 0.3 0.8
abCol' = labColor (-1.5/16) 0.3 0.75
bCol' = labColor (-4.5/16) 0.3 0.7

neutral = labColor 0 0 0.85

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
      
      intervalWedges :: Scale -> (Colour Double, Colour Double) -> (Double, Double) -> Diagram B
      intervalWedges scale (mainColor, tickColor) (innerRing, outerRing) = 
        let ringPath = (circle outerRing <> (circle innerRing # reversePath)) 
            ticksPath = B.intersection Winding ringPath $ B.difference Winding (circle (outerRing + 0.01)) intervalTicksPath
            notchesPath = B.difference Winding (circle (outerRing + 0.01)) $ foldr go mempty (toPCList scale)
            go :: PC -> Path V2 Double -> Path V2 Double
            go pc acc = 
              let rotAngle = fromIntegral (toInt pc) / 12.0
                  notch = rect 0.12 (outerRing + 0.01) # translateY (outerRing/2 + 0.005) # rotate (-rotAngle @@ turn)
              in  B.union Winding (notch <> acc)
        in  clipBy notchesPath $ 
              ticksPath # strokeP # fc tickColor # lcA transparent
              <> ringPath # strokeP # fc mainColor # lcA transparent
           
      pcWedge :: Int -> Diagram B
      pcWedge idx = 
          wedge 0.5 (rotate (-1/24 @@ turn) yDir) (1/12 @@ turn) # fc fillCol # lcA transparent
          where fillCol
                  | scaleA `contains` pc idx && scaleB `contains` pc idx = abCol
                  | scaleA `contains` pc idx = aCol
                  | scaleB `contains` pc idx = bCol
                  | otherwise = neutral

      intervalRings = intervalWedges scaleB (bCol, bCol') (0.6, 0.7) <> intervalWedges scaleA (aCol, aCol') (0.8, 0.9) -- & clipBy intervalTicksPath

      pcPie = foldr (go pcWedge) mempty [0..11] # clipBy pcTicksPath

      intervalTicksPath = foldr go (square 2) [0..5]
        where go :: Double -> Path V2 Double -> Path V2 Double
              go x acc = 
                let gridline = rect 0.03 2 # rotate (x * 1/12 @@ turn)
                in  B.difference Winding acc gridline
      pcTicksPath = intervalTicksPath # rotate (1/24 @@ turn)
  in pcPie <> intervalRings 

allMMTransitions :: Diagram B
allMMTransitions =
  vsep 1 [
    hsep 1 [ 
        hsep 0.1 [ inversePair (majorTriad pc0) (transpose x (majorTriad pc0))
                 | x <- pc <$> [0..6]]
      , hsep 0.1 [ inversePair (minorTriad pc0) (transpose x (minorTriad pc0))
                 | x <- pc <$> [0..6]]
    ]
    , hsep 0.1 [ inversePair (majorTriad pc0) (transpose x (minorTriad pc0))
               | x <- pc <$> [0..11]]
  ] # padX 1.05 # padY 1.1

inversePair :: Scale -> Scale -> Diagram B
inversePair scaleA scaleB = 
  let rootDiff = root scaleB - root scaleA
      scaleB' = transpose (-rootDiff) scaleA
      scaleA' = transpose (-rootDiff) scaleB
  in  if scaleA == scaleA' && scaleB' == scaleB
        then labeledTrans scaleA scaleB
      else vsep 0.3 [
        labeledTrans scaleA scaleB
      , labeledTrans scaleA' scaleB'
      ]

labeledTrans :: Scale -> Scale -> Diagram B
labeledTrans scaleA scaleB = vsep 0.2
  [ twoScaleClock scaleA scaleB 
  , text str # fontSize 8]
  where str = show (root scaleB - root scaleA) ++ pcsString (offsets scaleA) ++ pcsString (offsets scaleB)