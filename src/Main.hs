module Main where
import           AI
import           Io
import           TorusCheckers

main :: IO ()
main = do st <- readState
          printBoard $ board st
  --printBoard $ board $ makeAlphaBetaPruningMove 100 (State initialBoard  Red)
