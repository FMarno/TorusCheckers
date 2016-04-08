module Main where
import           AI
import           Io
import           TorusCheckers

main :: IO ()
main = do st <- readState
          let move = makeAlphaBetaPruningMove 100 st
          uncurry printMove move
  --printBoard $ board $ makeAlphaBetaPruningMove 100 (State initialBoard  Red)
