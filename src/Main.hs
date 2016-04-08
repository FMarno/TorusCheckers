module Main where
import           TorusCheckers
import AI

main :: IO ()
main = printBoard $ board $ makeAlphaBetaPruningMove 100 (State initialBoard  Red)
