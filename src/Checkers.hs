
module Checkers where

import Data.List

--Structures-------------------------------------

type Position = (Int, Int)

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
possibleMoves White p b@(Board s ps n) singlemoves =
											if singlemoves then singles else []-- ++ jumps
												where
													singles :: [[Position]]
													singles = [[x,y] | x <- [(fst p) +1,(fst p) -1 ] , y <- [(snd p) -1]]

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
initialBoard = Board 8 ([((x+(y `mod` 2),y),Red) | x <- [0,2,4,6], y <- [0,1,2]] ++
						[(((if y==7 || y ==5 then x else x-1),y),White) | x<-[1,3,5,7] , y <- [5,6,7]]) 0

--------------------------------------------------
printBoard :: Board -> String
printBoard board =
	concat $ concat $ intersperse ["\n"] $ breakUp (size board)
	$ (map (\x -> colourAt (pieces board) x)
	[(y,x)| x<-[0..(size board)-1], y<-[0..(size board)-1]])

colourAt :: [(Position, Colour)] -> Position -> String
colourAt pieces position
		| isPosition position White pieces 	= "|W|"
		| isPosition position Red pieces 	= "|R|"
		| otherwise								= "| |"

isPosition :: (Int, Int) -> Colour -> [(Position, Colour)] -> Bool
isPosition _ _ [] = False
isPosition position colour (piece:pieces) =
	equalPosition position colour piece || isPosition position colour pieces

equalPosition :: (Int, Int) -> Colour -> (Position, Colour) -> Bool
equalPosition position1 colour1 (position2, colour2) =
	position1 == position2 && colour1 == colour2

breakUp :: Int -> [a] -> [[a]]
breakUp l [] = [[]]
breakUp l xs 	| l >= length xs	= [xs]
				| otherwise 		= [take l xs] ++ breakUp l (drop l xs)

--------------------------------------------------
