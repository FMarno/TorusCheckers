module Main where
import           TorusCheckers

main :: IO ()
main = print $ possibleTurns Red 6 initialBoard
