module Music.PC (
  PC(), pc,
  Itv(), itv,
  Plus(..), Minus(..),
  pc0, pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pcT, pcE, pc10, pc11,
  up0, up1, up2, up3, up4, up5, up6, up7, up8, up9, upT, upE, up10, up11,
  dn0, dn1, dn2, dn3, dn4, dn5, dn6, dn7, dn8, dn9, dnT, dnE, dn10, dn11,
  unison, minorSecond, majorSecond, minorThird, majorThird, perfectFourth, tritone, perfectFifth, minorSixth, majorSixth, minorSeventh, majorSeventh,
  intervals, itvsString,
  Scale(..), isRoot, contains, index, scaleSize,
  toPCSet, toPCList, dyad, majorTriad, minorTriad, transpose, cMajorTriad, note,
  PCSet(), pcSet, sortedPCs
  ) where

import Data.List (nub, sort, elem, elemIndex)
import Data.Modular

type Mod12 = Mod Integer 12

newtype PC = PC 
  { pcMod12 :: Mod12 }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

newtype Itv = Itv 
  { ivlMod12 :: Mod12 }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

pc :: (Integral a) => a -> PC 
pc = PC . fromInteger . toInteger

itv :: (Integral a) => a -> Itv
itv = Itv . fromInteger . toInteger

instance Show PC where
  show (PC x)
    | x == 10 = "T"
    | x == 11 = "E"
    | otherwise = show x

instance Show Itv where
  show (Itv x) 
    | x == 10 = "+T"
    | x == 11 = "+E"
    | otherwise = "+" ++ show x

class Plus a b c where
  (+|) :: a -> b -> c

instance Plus PC PC PC where
  a +| b = a + b

instance Plus PC Itv PC where
  PC pcInt +| Itv ivlInt = PC $ pcInt + ivlInt
 
instance Plus Itv PC PC where
  Itv ivlInt +| PC pcInt = PC $ pcInt + ivlInt

instance Plus Itv Itv Itv where
  Itv ivlIntA +| Itv ivlIntB = Itv $ ivlIntA + ivlIntB 

class Minus a b c where
  (-|) :: a -> b -> c 

instance Minus PC PC Itv where
  pcA -| pcB = fromInteger . toInteger $ pcA - pcB

instance Minus PC Itv PC where
  pc -| itv = pc +| (-itv)

instance Minus Itv Itv Itv where
  (-|) = (-)

pc0, pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pcT, pcE, pc10, pc11 :: PC
pc0 = PC 0
pc1 = PC 1
pc2 = PC 2
pc3 = PC 3
pc4 = PC 4
pc5 = PC 5
pc6 = PC 6
pc7 = PC 7
pc8 = PC 8
pc9 = PC 9
pcT = PC 10 
pc10 = pcT
pcE = PC 11
pc11 = pcE

up0, up1, up2, up3, up4, up5, up6, up7, up8, up9, upT, upE, up10, up11 :: Itv
dn0, dn1, dn2, dn3, dn4, dn5, dn6, dn7, dn8, dn9, dnT, dnE, dn10, dn11 :: Itv
unison, minorSecond, majorSecond, minorThird, majorThird, perfectFourth, tritone, perfectFifth, minorSixth, majorSixth, minorSeventh, majorSeventh :: Itv
up0 = Itv 0
dn0 = up0
unison = up0
up1 = Itv 1
dnE = up1
dn11 = dnE
minorSecond = up1
up2 = Itv 2
dnT = up2
dn10 = dnT
majorSecond = up2
up3 = Itv 3
dn9 = up3
minorThird = up3
up4 = Itv 4
dn8 = up4
majorThird = up4
up5 = Itv 5
dn7 = up5
perfectFourth = up5
up6 = Itv 6
dn6 = up6
tritone = up6
up7 = Itv 7
perfectFifth = up7
dn5 = up7
up8 = Itv 8
minorSixth = up8
dn4 = up8
up9 = Itv 9
majorSixth = up9
dn3 = up9
upT = Itv 10
up10 = upT
minorSeventh = upT
dn2 = upT
upE = Itv 11 
up11 = upE
majorSeventh = upE
dn1 = upE

data Scale = Scale 
  {  root :: PC
  ,  offsets :: [Itv] 
  }
  deriving (Eq, Ord)

instance Show Scale where
  show (Scale root offsets) = 
    show root ++ "|" ++ show offsets

isRoot :: PC -> Scale -> Bool
isRoot pc (Scale root _) = pc == root

contains :: Scale -> PC -> Bool
contains scale pc = 
  pc `elem` toPCList scale

index :: PC -> Scale -> Maybe Int
index pc (Scale root offsets) = 
  let pcs = map (+| root) (up0:offsets)
   in pc `elemIndex` pcs

toPCSet :: Scale -> PCSet 
toPCSet = pcSet . toPCList

toPCList :: Scale -> [PC]
toPCList (Scale root offsets) = root : (map (root +|) offsets)

intervals :: [PC] -> [Itv]
intervals [] = []
intervals pcs = 
  zipWith (\a b -> b -| a) pcs (drop 1 $ cycle pcs)

majorChordItvs, minorChordItvs :: [Itv]
majorChordItvs = [up4, up7]
minorChordItvs = [up3, up7]

majorTriad :: PC -> Scale
majorTriad root = Scale root majorChordItvs

minorTriad :: PC -> Scale
minorTriad root = Scale root minorChordItvs

note :: PC -> Scale
note pc = Scale pc []

scaleSize :: Scale -> Int
scaleSize (Scale _ offsets) = 1 + length offsets

class Dyad a b where  
  dyad :: a -> b -> Scale

instance Dyad PC Itv where
  dyad :: PC -> Itv -> Scale
  dyad root offset = Scale root [offset]

instance Dyad Itv PC where
  dyad :: Itv -> PC -> Scale
  dyad offset root = Scale root [offset]

instance Dyad PC PC where
  dyad :: PC -> PC -> Scale
  dyad a b = Scale a [b -| a]

cMajorTriad = Scale pc0 majorChordItvs

transpose :: Itv -> Scale -> Scale
transpose transposeOffset (Scale root offsets) = Scale (root +| transposeOffset) offsets

itvsString :: [Itv] -> String
itvsString ups 
  | ups == majorChordItvs = "M"
  | ups == minorChordItvs = "m"
  | otherwise = "?"

data PCSet = PCSet 
  { pcSetPCs :: [PC] }
  deriving (Show, Eq, Ord)

sortedPCs :: PCSet -> [PC]
sortedPCs = pcSetPCs

pcSet :: [PC] -> PCSet 
pcSet = PCSet . nub . sort
