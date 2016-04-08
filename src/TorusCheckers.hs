module TorusCheckers where

import           Data.List
import           Debug.Trace

--Structures-------------------------------------

type Position = Int

type Turn = [Position]

data Colour = White | Red | Non
	deriving (Eq, Show)

other :: Colour -> Colour
other White = Red
other Red = White
other Non = Non

data Board = Board {
					size :: Int,
					pieces :: [(Position, Colour)],
					playNum :: Int
} deriving (Show)

data State = State {
					board :: Board,
					turn :: Colour
}
--------------------------------------------------

possibleTurns :: Colour -> Position -> Board -> ([Turn], Bool)
possibleTurns col pos b@(Board s ps n) | null jturns = (singleTurns, True)
									 | otherwise = (jturns, False)
									 where
									 	jturns = filter (\x -> length x /= 1) $ jumpTurns col pos b
										singleTurns = map (pos:) $ breakUp 1 $ filter (emptySpace b) $ steps col pos s

jumpTurns :: Colour -> Position -> Board -> [Turn]
jumpTurns col pos b@(Board size ps _) | null  validJumps = [[pos]]
 																	| otherwise = map (pos:) $ concatMap (\target -> jumpTurns col target (removePiece (between pos target col size) $ removePiece pos $ addPiece col target b )) validJumps
																		where
																			validJumps = lJump ++ rJump
																			lJump = [head possibleJumps | adjacentOpponent col pos b True && emptySpace b (head possibleJumps)]
																			rJump =  [possibleJumps !! 1 | adjacentOpponent col pos b False && emptySpace b (possibleJumps !! 1)]
																			possibleJumps = jumps col pos size


adjacentOpponent :: Colour -> Position -> Board -> Bool -> Bool
adjacentOpponent col startPos b@(Board size ps _) left | left = not (emptySpace b (head s)) && (head s, other col) `elem` ps
										   | otherwise = not (emptySpace b (s !! 1)) && (s !! 1, other col) `elem` ps
										   where s = steps col startPos size

emptySpace :: Board -> Position -> Bool
emptySpace (Board _ ps _ ) pos = pos `notElem` map fst ps


jumps :: Colour -> Position -> Int -> [Position]
jumps colour pos size = flanks (twoForward colour pos size) size

flanks :: Position -> Int -> [Position]
flanks pos size | localPos == hsize - 1 = [pos-1,pos-(hsize-1)]
				| localPos == 0 = [pos+hsize-1, pos+1]
				| otherwise = [pos-1, pos+1]
			where
				localPos = (pos - 1) `mod` hsize
				hsize = size `div` 2

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

makeMove :: Colour -> (Turn, Bool) -> Board -> Board
makeMove col (turn, isSingleMove) (Board s ps pn)
		| isSingleMove = addPiece col (turn !! 1) $ removePiece (head turn) (Board s ps (pn+1))
		| otherwise = makeJumpMove col turn (Board s ps (pn+1))

makeJumpMove :: Colour -> Turn -> Board -> Board
makeJumpMove _ [t] b = b
makeJumpMove col (t:t':ts) b@(Board size _ _) = makeJumpMove col (t':ts) onejump
	where
		onejump = removePiece (between t t' col size) $ removePiece t $ addPiece col t' b

between :: Position -> Position -> Colour -> Int -> Int
between start finish col size | head possibleJumps == finish = head possibleSteps
															| otherwise = possibleSteps !! 1
															where
																possibleJumps = jumps col start size
																possibleSteps = steps col start size

addPiece :: Colour -> Position -> Board -> Board
addPiece c p (Board s ps n) = Board s ((p,c) : ps) n

removePiece :: Position -> Board -> Board
removePiece p (Board s ps n) = Board s (rP p ps) n
									where
										rP :: Position -> [(Position, Colour)] -> [(Position, Colour)]
										rP _ [] = []
										rP p (x:xs) = if fst x == p then xs else x : rP p xs

isColourAt :: Colour -> Position -> Board -> Bool
isColourAt c p (Board s ps n) = (p,c) `elem` ps

evaluateBoard :: Board -> Int
evaluateBoard (Board s ps n) = numberOfPieces Red ps - numberOfPieces White ps

findWinner :: Board -> Colour
findWinner (Board size ps pn) | numberOfPieces Red ps > numberOfPieces White ps = Red
															| numberOfPieces White ps > numberOfPieces Red ps = White
															| otherwise = Non

numberOfPieces :: Colour -> [(Position, Colour)] -> Int
numberOfPieces colour ps = length $ filter (\x -> snd x == colour) ps
--------------------------------------------------

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
