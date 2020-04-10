module Music.PC (
  PC(), pc, toInt,
  pc0, pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pcT, pcE,
  intervals, pcsString,
  Scale(..), isRoot, contains, index,
  toPCSet, toPCList, majorTriad, minorTriad, transpose, cMajChord,
  PCSet(), pcSet, sortedPCs
  ) where

import Data.List (nub, sort, elem, elemIndex)

data PC = PC 
  { pcInt :: Int }
  deriving (Eq, Ord)

toInt :: PC -> Int
toInt = pcInt

instance Show PC where
  show (PC x)
    | x == 10 = "T"
    | x == 11 = "E"
    | otherwise = show x

pc :: Int -> PC 
pc x = PC $ x `mod` 12

instance Num PC where 
  (PC x) + (PC y) = pc $ x + y
  (PC x) - (PC y) = pc $ x - y
  (PC x) * (PC y) = pc $ x * y
  negate (PC x) = pc $ negate x
  abs = id
  signum (PC x) = PC (signum x)
  fromInteger = pc . fromInteger

pc0, pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pcT, pcE :: PC
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
pcE = PC 11

data Scale = Scale 
  {  root :: PC
  ,  offsets :: [PC] 
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
  let pcs = map (+root) offsets
   in pc `elemIndex` pcs

toPCSet :: Scale -> PCSet 
toPCSet = pcSet . toPCList

toPCList :: Scale -> [PC]
toPCList (Scale root offsets) = root : (map (+root) offsets)

intervals :: [PC] -> [PC]
intervals [] = []
intervals pcs = 
  zipWith (\a b -> b - a) pcs (drop 1 $ cycle pcs)

majorChordPCs, minorChordPCs :: [PC]
majorChordPCs = [pc4, pc7]
minorChordPCs = [pc3, pc7]

majorTriad :: PC -> Scale
majorTriad root = Scale root majorChordPCs

minorTriad :: PC -> Scale
minorTriad root = Scale root minorChordPCs

cMajChord = Scale pc0 majorChordPCs

transpose :: PC -> Scale -> Scale
transpose amount (Scale root offsets) = Scale (root + amount) offsets

pcsString :: [PC] -> String
pcsString pcs 
  | pcs == majorChordPCs = "M"
  | pcs == minorChordPCs = "m"
  | otherwise = "?"

data PCSet = PCSet 
  { pcSetPCs :: [PC] }
  deriving (Show, Eq, Ord)

sortedPCs :: PCSet -> [PC]
sortedPCs = pcSetPCs

pcSet :: [PC] -> PCSet 
pcSet = PCSet . nub . sort
