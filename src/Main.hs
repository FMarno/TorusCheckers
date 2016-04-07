module Main where
import           TorusCheckers

main :: IO ()
main = print $ possibleTurns Red 14 initialBoard True
