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

    assertEqual "torus from White 1" [29,30] (steps White 1 8),
    assertEqual "torus from White 1" [29,30] (steps White 1 8),
    assertEqual "torus from White 1" [29,30] (steps Red 1 8),
    assertEqual "torus from White 1" [29,30] (steps Red 1 8)
    ]

run :: IO Counts
run  = runTestTT tests
