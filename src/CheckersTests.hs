module CheckerTests where

import Test.HUnit
import TorusCheckers


tests = TestList $ map TestCase [
    assertEqual "steps from red 6, normal" [9,10] (steps Red 6 8),
    assertEqual "steps from red 2, normal" [6,7] (steps Red 2 8),
    assertEqual "torus from red 13" [20,17] (steps Red 13 8),
    assertEqual "torus from red 12" [16,13] (steps Red 12 8),
    assertEqual "torus from red 29" [4,1] (steps Red 29 8),
    assertEqual "torus from red 28" [32,29] (steps Red 28 8),
    assertEqual "torus from red 32" [3,4] (steps Red 32 8),

    assertEqual "steps from White 25, normal" [21,22] (steps White 25 8),
    assertEqual "steps from White 30" [25,26] (steps White 30 8),
    assertEqual "torus from White 21" [20,17] (steps White 21 8),
    assertEqual "torus from White 20" [16,13] (steps White 20 8),
    assertEqual "torus from White 4" [32,29] (steps White 4 8),
    assertEqual "torus from White 1" [29,30] (steps White 1 8),

    assertEqual "jump from White 30" [21,23] (steps White 30 8),
    assertEqual "jump from White 25" [20,18] (steps White 25 8),
    assertEqual "jump from White 21" [16,14] (steps White 21 8),
    assertEqual "jump from White 24" [15,13] (steps White 24 8),
    assertEqual "jump from White 20" [11,9] (steps White 20 8),
    assertEqual "jump from White 2" [25,27] (steps White 2 8),
    assertEqual "jump from White 1" [28,26] (steps White 1 8),
    assertEqual "jump from White 51" [32,30] (steps White 5 8),
    assertEqual "jump from White 4" [27,25] (steps White 4 8),
    assertEqual "jump from White 8" [31,29] (steps White 8 8),

    assertEqual "jump from Red 8" [15,13] (steps Red 8 8),
    assertEqual "jump from Red 4" [11,9] (steps Red 4 8),
    assertEqual "jump from Red 5" [16,14] (steps Red 5 8),
    assertEqual "jump from Red 1" [12,10] (steps Red 1 8),
    assertEqual "jump from Red 2" [9,11] (steps Red 2 8),
    assertEqual "jump from Red 20" [27,25] (steps Red 20 8),
    assertEqual "jump from Red 24" [31,29] (steps Red 24 8),
    assertEqual "jump from Red 21" [32,30] (steps Red 21 8),
    assertEqual "jump from Red 25" [4,2] (steps Red 25 8),
    assertEqual "jump from Red 30" [5,7] (steps Red 30 8)

    ]


main :: IO Counts
main = runTestTT tests
