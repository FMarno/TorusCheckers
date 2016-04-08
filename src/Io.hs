module Io where
import           TorusCheckers

readState :: IO State
readState  = do g <- getLine
                p <- getLine
                r <- getLine
                w <- getLine
                return (stringsToState g p r w)

stringsToState :: String -> String -> String -> String -> State
stringsToState g p r w = State newBoard turn
        where
          newBoard = Board s ps (read (player !! 1) :: Int)
          s = if ((game !! 1) == "8") && ((game !! 2) == "8") then 8 else error "I didn't do extensions"
          ps = piecesFromString Red red ++ piecesFromString White white
          turn = if player !! 2 == "0" then Red else White
          game = words g
          player = words p
          red = words r
          white = words w

piecesFromString :: Colour -> [String] -> [(Position, Colour)]
piecesFromString col (x:xs) = map (\z -> (read z :: Int, col)) xs

printMove :: IO()
printMove = undefined

printState :: IO()
printState = undefined
