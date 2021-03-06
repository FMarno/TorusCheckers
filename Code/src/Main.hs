module Main where
import           AI
import           Io
import           TorusCheckers

main :: IO ()
main = do st <- readState -- read the state
          let move = makeAlphaBetaPruningMove 8 st --find the move
          uncurry printMove move -- print the move
          printState $ fst move
          --relax

          --putStrLn $ show st
          --printBoard $ board st
          --putStrLn " "
          --printBoard $ board $ fst move
          --putStrLn $ printPositions 8
          --putStrLn $ show $ allMovesOf Red $ board st
