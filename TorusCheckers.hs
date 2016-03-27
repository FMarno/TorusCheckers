module TorusCheckers where

import Data.List

--Structures-------------------------------------

type Position = Int

type Move = (Position, Position)

data Colour = White | Red | Non
	deriving (Eq, Show)

data Board = Board {
					size :: Int,
					pieces :: [(Position, Colour)],
					play_num :: Int
} deriving (Show)

data State = State {
					board :: Board,
					turn :: Colour
}

data GameTree = GameTree {
										game_board :: Board,
										game_turn :: Colour,
										next_moves :: [(Position, GameTree)]
}
--------------------------------------------------

positionsOfColour :: Colour -> Board -> [Position]
positionsOfColour Non board = []
positionsOfColour colour board = map fst $ filter (\x -> snd x == colour) (pieces board)

possibleMoves :: Colour -> Position -> Board -> Bool -> [[Position]]
possibleMoves Red pos (Board s ps n) singlemoves = undefined

--find the two possible moves that are a space away
steps :: Colour -> Position -> Int -> [Position]
steps Red pos size
	| pos > (size^2 `div` 2) - hsize = steps Red (pos-(size^2 `div` 2)) size
	| localPos < hsize = if localPos == hsize - 1 then [pos+hsize, pos+1] else [pos+hsize, pos+hsize+1] --oddRows
 	| otherwise =	if localPos == hsize then [pos+size-1,pos+hsize] else [pos+hsize-1, pos+hsize] --even rows
									 where
										 localPos = ((pos - 1) `mod` size)
									 	 hsize = size `div` 2
steps White pos size
	| pos <= hsize = steps White (pos+(size^2 `div` 2)) size
	| localPos < hsize = if localPos == hsize - 1 then [pos-hsize, pos-size+1] else [pos-hsize, pos-hsize+1] --oddRows
	| otherwise =	if localPos == hsize then [pos-1,pos-hsize] else [pos-hsize-1, pos-hsize] --even rows
									 where
										 localPos = ((pos - 1) `mod` size)
									 	 hsize = size `div` 2

movePiece :: Colour -> Position -> Board -> [Position] -> Board
movePiece = undefined

addPiece :: Colour -> Position -> Board -> Board
addPiece c p (Board s ps n) = Board s ((p,c) : ps) n

removePiece :: Position -> Board -> Board
removePiece p (Board s ps n) = Board s (rP p ps) n
									where
										rP :: Position -> [(Position, Colour)] -> [(Position, Colour)]
										rP _ [] = []
										rP p (x:xs) = if fst x == p then xs else x:(rP p xs)

isColourAt :: Colour -> Position -> Board -> Bool
isColourAt c p (Board s ps n) = elem (p,c) ps

evaluateBoard :: Colour -> Board -> Int
evaluateBoard c (Board s ps n) = numberOfPieces Red - numberOfPieces White
						where
							numberOfPieces :: Colour -> Int
							numberOfPieces colour = length $ filter (\x -> snd x == colour) ps

--------------------------------------------------

initialBoard :: Board
initialBoard = Board 8 ([(r, Red) | r <- [1..12]] ++ [(w,White) | w <- [21..32]]) 0

printPositions :: Int -> String
printPositions size = concat $ intersperse "\n" $ map show
        $ breakUp (size `div` 2) [1..((size^2) `div` 2)]

printBoard :: Board -> IO()
printBoard (Board size ps _) =
  putStrLn $ concat $ intersperse "\n" $map concat $
    mapAlternative (map (\z->"| |"++z)) (map (++"| |")) True $ breakUp (size`div`2) $
    [x | y<-[1..((size^2)`div`2)],
      x<- if elem (y, White) ps
            then ["|W|"]
          else if elem (y, Red) ps
            then ["|R|"] else ["| |"]
    ]

mapAlternative :: (a -> b) -> (a -> b) -> Bool -> [a] -> [b]
mapAlternative _ _ _ [] = []
mapAlternative f g which (x:xs) | which = (f x) : mapAlternative f g False xs
																| otherwise = (g x) : mapAlternative f g True xs

breakUp :: Int -> [a] -> [[a]]
breakUp l [] = []
breakUp l xs 	| l >= length xs	= [xs]
				| otherwise 		= [take l xs] ++ breakUp l (drop l xs)
