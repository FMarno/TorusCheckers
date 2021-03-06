module TorusCheckers where

import           Data.List
import           Debug.Trace

--Structures-------------------------------------

type Position = Int

type Turn = [Position]

data Colour = White | Red | Non deriving (Eq, Show)

other :: Colour -> Colour
other White = Red
other Red = White
other Non = Non

data Board = Board {
  size    :: Int,
  pieces  :: [(Position, Colour)],
  playNum :: Int
} deriving (Show)

data State = State {
  board :: Board,
  turn  :: Colour
} deriving (Show)
--------------------------------------------------
-- find all the possible move available for a colour
-- the bool returns is turn if its a step
-- false if its a jump
allMovesOf :: Colour -> Board -> [(Turn, Bool)]
allMovesOf col (Board _ _ 100) = []
allMovesOf col b@(Board s ps n) = if elem False (map snd $ piecesMoves) then filter (\x -> not $ snd x) piecesMoves else piecesMoves
										where
											piecesMoves = concatMap (zippish . (\x -> possibleTurns col x b)) (allOf col ps)
											zippish (as, bool) = map (\z -> (z, bool)) as

-- find all the possible turns from a position for a colour
possibleTurns :: Colour -> Position -> Board -> ([Turn], Bool)
possibleTurns col pos b@(Board s ps n) | null jturns = (singleTurns, True)
									 | otherwise = (jturns, False)
									 where
									 	jturns = filter (\x -> length x /= 1) $ jumpTurns col pos b
										singleTurns = map (pos:) $ breakUp 1 $ filter (emptySpace b) $ steps col pos s

-- find all the turns that involve jumping from a position for a colour
jumpTurns :: Colour -> Position -> Board -> [Turn]
jumpTurns col pos b@(Board size ps _)
			| null  validJumps = [[pos]]
			| otherwise = map (pos:) $ concatMap (\target -> jumpTurns col target (removePiece (between pos target col size) $ removePiece pos $ addPiece col target b )) validJumps
			where
				validJumps = lJump ++ rJump
				lJump = [head possibleJumps | adjacentOpponent col pos b True && emptySpace b (head possibleJumps)]
				rJump =  [possibleJumps !! 1 | adjacentOpponent col pos b False && emptySpace b (possibleJumps !! 1)]
				possibleJumps = jumps col pos size

-- test if an index is beside an opponent on a certain side
adjacentOpponent :: Colour -> Position -> Board -> Bool -> Bool
adjacentOpponent col startPos b@(Board size ps _) left | left = not (emptySpace b (head s)) && (head s, other col) `elem` ps
										   | otherwise = not (emptySpace b (s !! 1)) && (s !! 1, other col) `elem` ps
										   where s = steps col startPos size

-- check if an index is empty
emptySpace :: Board -> Position -> Bool
emptySpace (Board _ ps _ ) pos = pos `notElem` map fst ps

-- find the two index a jump away from a index
jumps :: Colour -> Position -> Int -> [Position]
jumps colour pos size = flanks (twoForward colour pos size) size

-- find the indexes either side of a index
flanks :: Position -> Int -> [Position]
flanks pos size | localPos == hsize - 1 = [pos-1,pos-(hsize-1)]
				| localPos == 0 = [pos+hsize-1, pos+1]
				| otherwise = [pos-1, pos+1]
			where
				localPos = (pos - 1) `mod` hsize
				hsize = size `div` 2

-- find the index directly infront of a position for a colour
-- this would have a single white tile inbetween
twoForward :: Colour -> Position -> Int -> Position
twoForward Red pos size | pos > ((size^2)`div`2) - size = pos - ((size^2)`div`2 - size)
						| otherwise = pos + size
twoForward White pos size | pos > size = pos - size
						  | otherwise = (pos + ((size^2)`div`2)) - size

--find the two possible moves that are a space away
steps :: Colour -> Position -> Int -> [Position]
steps Red pos size
	| pos > (size^2 `div` 2) - hsize = steps Red (pos-(size^2 `div` 2)) size
	| localPos < hsize = if localPos == hsize - 1 then [pos+hsize, pos+1] else [pos+hsize, pos+hsize+1] --oddRows
 	| otherwise =	if localPos == hsize then [pos+size-1,pos+hsize] else [pos+hsize-1, pos+hsize] --even rows
									 where
										 localPos = (pos - 1) `mod` size
									 	 hsize = size `div` 2
steps White pos size
	| pos <= hsize = steps White (pos+(size^2 `div` 2)) size
	| localPos < hsize = if localPos == hsize - 1 then [pos-hsize, pos-size+1] else [pos-hsize, pos-hsize+1] --oddRows
	| otherwise =	if localPos == hsize then [pos-1,pos-hsize] else [pos-hsize-1, pos-hsize] --even rows
									 where
										 localPos = (pos - 1) `mod` size
									 	 hsize = size `div` 2

--RED MOVES FIRST
-- apply a turn to the board and return resulting board
makeMove :: Colour -> (Turn, Bool) -> Board -> Board
makeMove col (turn, isSingleMove) (Board s ps pn)
		| isSingleMove = addPiece col (turn !! 1) $ removePiece (head turn) (Board s ps newPn)
		| otherwise = makeJumpMove col turn (Board s ps newPn)
		where newPn = if col == White then pn+1 else pn

-- apply a turn with jumps to the board and return the resulting board
makeJumpMove :: Colour -> Turn -> Board -> Board
makeJumpMove _ [t] b = b
makeJumpMove col (t:t':ts) b@(Board size _ _) = makeJumpMove col (t':ts) onejump
	where
		onejump = removePiece (between t t' col size) $ removePiece t $ addPiece col t' b

-- finds the index between two positions
between :: Position -> Position -> Colour -> Int -> Int
between start finish col size | head possibleJumps == finish = head possibleSteps
															| otherwise = possibleSteps !! 1
															where
																possibleJumps = jumps col start size
																possibleSteps = steps col start size

-- adds a piece of a certain colour to the board at a given position
addPiece :: Colour -> Position -> Board -> Board
addPiece c p (Board s ps n) = Board s ((p,c) : ps) n

-- remove the piece at a given position from the board
removePiece :: Position -> Board -> Board
removePiece p (Board s ps n) = Board s (rP p ps) n
									where
										rP :: Position -> [(Position, Colour)] -> [(Position, Colour)]
										rP _ [] = []
										rP p (x:xs) = if fst x == p then xs else x : rP p xs

-- checks if a colour is at a position on a board
isColourAt :: Colour -> Position -> Board -> Bool
isColourAt c p (Board s ps n) = (p,c) `elem` ps

-- evaluate the board state for a given colour
-- just the difference in the number of pieces
evaluateBoard :: Colour -> Board -> Int
evaluateBoard Red (Board s ps n) | redPs /= 0 = redPs - whitePs
																 | otherwise = minBound
			where
				redPs = numberOfPieces Red ps
				whitePs = numberOfPieces White ps
evaluateBoard White (Board s ps n) | whitePs /= 0 = whitePs - redPs
																	 | otherwise = minBound
			where
				redPs = numberOfPieces Red ps
				whitePs = numberOfPieces White ps

-- find the winner or if its a draw from a board
findWinner :: Board -> Colour
findWinner (Board size ps pn) | numberOfPieces Red ps > numberOfPieces White ps = Red
															| numberOfPieces White ps > numberOfPieces Red ps = White
															| otherwise = Non

-- find the number of pieces of a given colour in a (position, colour) list
numberOfPieces :: Colour -> [(Position, Colour)] -> Int
numberOfPieces colour ps = length $ filter (\x -> snd x == colour) ps

-- find the position of all of a given colour
-- in a list of (position, colour) tuples
allOf :: Colour -> [(Position, Colour)]-> [Position]
allOf colour ps = map fst $ filter (\x -> snd x == colour) ps

-- utility things--------------------------------

initialBoard :: Board
initialBoard = Board 8 ([(r, Red) | r <- [1..12]] ++ [(w,White) | w <- [21..32]]) 0

printPositions :: Int -> String
printPositions size = intercalate "\n" $ map show
        $ breakUp (size `div` 2) [1..((size^2) `div` 2)]

printBoard :: Board -> IO()
printBoard (Board size ps _) =
  putStrLn $ intercalate "\n" $ map concat $
    mapAlternative (map (\z->"| |"++z)) (map (++"| |")) True $ breakUp (size`div`2)
    [x | y<-[1..((size^2)`div`2)],
      x<- if (y, White) `elem` ps
            then ["|W|"]
          else if (y, Red) `elem` ps
            then ["|R|"] else ["| |"]
    ]

mapAlternative :: (a -> b) -> (a -> b) -> Bool -> [a] -> [b]
mapAlternative _ _ _ [] = []
mapAlternative f g which (x:xs) | which = f x : mapAlternative f g False xs
																| otherwise = g x : mapAlternative f g True xs

breakUp :: Int -> [a] -> [[a]]
breakUp l [] = []
breakUp l xs 	| l >= length xs	= [xs]
				| otherwise 		= take l xs : breakUp l (drop l xs)

positionsOfColour :: Colour -> Board -> [Position]
positionsOfColour Non board = []
positionsOfColour colour board = map fst $ filter (\x -> snd x == colour) (pieces board)

jumpBoard = Board 8 [(18,White), (14,Red) ,(15, Red), (5,Red),(7,Red), (32, Red), (23, Red)] 0
