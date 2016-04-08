module AI where

import Data.List
import Data.Ord
import Debug.Trace
import Data.Function
import TorusCheckers

data GameTree = GameTree {
                    gameBoard :: Board,
                    gameTurn :: Colour,
                    nextMoves :: [((Turn, Bool), GameTree)]
                    }
    deriving (Show)

--1.----------------------------------------------------------
makeAlphaBetaPruningMove :: State -> State
makeAlphaBetaPruningMove (State b@(Board size ps play) turn)
   = State newBoard (other turn)
    where newBoard = makeMove bestMove b
          bestMove = getBestAlphaBetaMove 5 $ buildAlphaBetaTree generateAlphaBetaMoves b turn

--2.----------------------------------------------------------
buildAlphaBetaTree :: (Board -> Colour -> [(Turn, Bool)])-> Board -> Colour -> GameTree
buildAlphaBetaTree generate board colour =
    let moves = generate board colour in GameTree board colour (makeNextStates moves)
  where
    makeNextStates :: [(Turn, Bool)] -> [((Turn, Bool), GameTree)]
    makeNextStates = map (\t -> (t, buildAlphaBetaTree generate (makeMove colour t board) (other colour)))

--3.----------------------------------------------------------

generateAlphaBetaMoves ::  Board -> Colour -> [(Turn, Bool)]
generateAlphaBetaMoves  board colour = allMovesOf colour board

--4.----------------------------------------------------------
--TODO here down
getBestAlphaBetaMove :: Int-> GameTree -> (Turn, Bool)
getBestAlphaBetaMove depth gameTree = trace (show (gameTurn gameTree) ++ ":" ++ show choice) choice
          where getTurn (t,tree) = t
                choice = getTurn $ getTopAlphaBetaMove depth (gameTurn gameTree) (nextMoves gameTree)


--5.----------------------------------------------------------

getTopAlphaBetaMove :: Int -> Colour -> [(Position, GameTree)] -> (Position, GameTree)
getTopAlphaBetaMove  depth _ [p] = p
getTopAlphaBetaMove  depth colour pieces = trace (show $ zip (map selectFirst values) (map fst pieces)) choice
   where values = map (\(_, tree) -> alphaBetaPruning  depth colour False tree (minBound, maxBound)) pieces
         choice = fst $ maximumBy (compare `on` snd) $ zip pieces values

--6.----------------------------------------------------------

alphaBetaPruning ::  Int -> Colour -> Bool -> GameTree -> (Int, Int) -> (Int,Int,Int)
alphaBetaPruning  depth colour maxPlayer (GameTree board turn []) (alpha, beta) = (evaluateBoard colour board, alpha, beta)
alphaBetaPruning  0 colour maxPlayer (GameTree board turn _) (alpha, beta) = (evaluateBoard colour board, alpha, beta)
alphaBetaPruning  depth colour maxPlayer (GameTree board game_turn moves) (alpha, beta)
       | checkWon board == colour
          = (100 , alpha, beta)
       | checkWon board == other colour
          = (-100, alpha, beta)
       | otherwise
          = h
          where (treeOfMoves : allOtherPossibleMoves) = map treeOf moves
                treeOf (p,tree) = tree
                valueOfFirstOppselectFirstntChild = alphaBetaPruning  (depth-1) colour (not maxPlayer) treeOfMoves (alpha,beta)
                h = foldl' (accumulate  depth colour maxPlayer) valueOfFirstOppselectFirstntChild allOtherPossibleMoves

accumulate ::  Int-> Colour-> Bool -> (Int, Int, Int) -> GameTree -> (Int, Int, Int)
accumulate  depth colour maxPlayer input@(value, alpha, beta) gameTree
   | beta <= alpha = input
   | otherwise     = (v, newalpha, newbeta)
      where operator
              | maxPlayer = max
              | otherwise = min
            v = operator newh value
            (newh, newa, newb) = alphaBetaPruning (depth-1) colour (not maxPlayer) gameTree (alpha, beta)
            newalpha
              | maxPlayer = operator v newa
              | otherwise = alpha
            newbeta
              | maxPlayer = beta
              | otherwise = operator v newb

selectFirst :: (Int, Int,Int) -> Int
selectFirst (a,b,c) = a
