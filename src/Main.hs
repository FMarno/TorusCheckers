import TorusCheckers

main :: IO ()
main = do putStrLn $ show $ possibleMoves Red 14 initialBoard True
