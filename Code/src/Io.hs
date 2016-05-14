module Io where
import           TorusCheckers
-- read the state in
readState :: IO State
readState  = do g <- getLine
                p <- getLine
                r <- getLine
                w <- getLine
                return (stringsToState g p r w)

-- convert the 4 lines to a state object
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

-- convert a string into a list representing pieces
piecesFromString :: Colour -> [String] -> [(Position, Colour)]
piecesFromString col (x:y:xs) = map (\z -> (read z :: Int, col)) xs

-- print out the final line signifying my move
printMove :: State -> Turn -> IO()
printMove (State b@(Board s ps pn) turn) move = putStrLn $ "m " ++ show pn ++ (if turn == Red then " 0 " else " 1 ") ++ thing
    where
      thing = if null move then "0 " ++ winner else moves
      winner | findWinner b == turn = "1"
             | findWinner b == other turn = "-1"
             | otherwise = "0"
      moves = unwords (map show move)

-- would have printed the input for the next move :(
printState :: State -> IO()
printState (State (Board s ps pn) turn) = do putStrLn "g 8 8 100"
                                             putStrLn $ "p " ++ show pn ++ (if turn == Red then " 0" else " 1")
                                             putStrLn $ "r " ++ show ( length reds) ++ " " ++ unwords reds
                                             putStrLn $ "w " ++ show ( length whites) ++ " " ++ unwords whites
                                             where
                                              reds = map (show . fst) $ filter (\z -> snd z == Red) ps
                                              whites = map (show . fst) $ filter (\z -> snd z == White) ps
