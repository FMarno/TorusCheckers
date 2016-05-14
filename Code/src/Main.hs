module Main where
import           AI
import           Io
import           TorusCheckers

main :: IO ()
main = do st <- readState -- read the state
          let move = makeAlphaBetaPruningMove 100 st --find the move
          uncurry printMove move -- print the move
          -- relax
