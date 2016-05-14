module Minimax where

import           Data.Function
import           Data.List
import           Data.Ord
import           Debug.Trace
import           TorusCheckers

-- recursive structure that lazily represents all possible game states
data GameTree = GameTree {
                    gameBoard :: Board,
                    gameTurn  :: Colour,
                    nextMoves :: [((Turn, Bool), GameTree)]
                    }
    deriving (Show)

--1.----------------------------------------------------------
-- given a depth and a state return a new state and the turn that was applied to it
makeMinimaxMove :: Int -> State -> (State, Turn)
makeMinimaxMove depth s@(State b@(Board size ps play) turn)
   | null nextMove = (s, [])
   | otherwise = (State newBoard (other turn), fst bestMove)
    where newBoard = makeMove turn bestMove b
          bestMove = getBestMinimaxMove depth $ buildMinimaxTree generateMinimaxMoves b turn
          nextMove = generateMinimaxMoves b turn

--2.----------------------------------------------------------
-- build the tree to search
buildMinimaxTree :: (Board -> Colour -> [(Turn, Bool)])-> Board -> Colour -> GameTree
buildMinimaxTree generate board colour =
    let moves = generate board colour in GameTree board colour (makeNextStates moves)
  where
    makeNextStates :: [(Turn, Bool)] -> [((Turn, Bool), GameTree)]
    makeNextStates = map (\t -> (t, buildMinimaxTree generate (makeMove colour t board) (other colour)))

--3.----------------------------------------------------------
-- generate all the possible moves for a colour on a given board
generateMinimaxMoves ::  Board -> Colour -> [(Turn, Bool)]
generateMinimaxMoves  board colour = allMovesOf colour board

--4.----------------------------------------------------------
--remove the wanted information from the getTopMinimaxMove function
getBestMinimaxMove :: Int-> GameTree -> (Turn, Bool)
getBestMinimaxMove depth gameTree = trace ("choice " ++ show choice) choice
          where getTurn (t,tree) = t
                choice = getTurn $ getTopMinimaxMove depth (gameTurn gameTree) (nextMoves gameTree)


--5.----------------------------------------------------------
--read the max move from first layer of children
getTopMinimaxMove :: Int -> Colour -> [((Turn, Bool), GameTree)] -> ((Turn, Bool), Int)
getTopMinimaxMove _ _ [p] = (fst p, 0)
getTopMinimaxMove depth colour gameTree = maximumBy (compare `on` snd) (trace (show choices ++ " " ++ show colour) choices)
                                  where choices = zip (map fst gameTree) $ map (\turn -> minimax (depth-1) colour False turn) $ (map snd gameTree)

--6.----------------------------------------------------------
minimax ::  Int -> Colour -> Bool -> GameTree -> Int
-- no available moves from this part
minimax _ aiColour _ (GameTree board _ []) = evaluateBoard aiColour board
-- reached search depth
minimax 0 aiColour _ (GameTree board _ _) = evaluateBoard aiColour board
-- evaluate a node
minimax depth aiColour maxPlayer gameTree
        | maxPlayer = maximum $ map (minimax (depth-1) aiColour False ) $ map snd $ nextMoves gameTree
        | not maxPlayer = minimum $ map (minimax (depth-1) aiColour True)  $ map snd $ nextMoves gameTree
