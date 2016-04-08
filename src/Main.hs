module Main where
import           AI
import           TorusCheckers

main :: IO ()
main = printBoard $ board $ makeAlphaBetaPruningMove 100 (State initialBoard  Red)
