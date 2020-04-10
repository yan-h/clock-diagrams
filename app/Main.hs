module Main where

import Draw
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Music.PC


main :: IO ()
main = mainWith allMMTransitions
