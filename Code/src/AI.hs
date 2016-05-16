module AI where

import           Data.Function
import           Data.List
import           Data.Ord
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
makeAlphaBetaPruningMove :: Int -> State -> (State, Turn)
makeAlphaBetaPruningMove depth s@(State b@(Board size ps play) turn)
   | null nextMove = (s, [])
   | otherwise = (State newBoard (other turn), fst bestMove)
    where newBoard = makeMove turn bestMove b
          bestMove = getBestAlphaBetaMove depth $ buildAlphaBetaTree generateAlphaBetaMoves b turn
          nextMove = generateAlphaBetaMoves b turn

--2.----------------------------------------------------------
-- build the tree to search
buildAlphaBetaTree :: (Board -> Colour -> [(Turn, Bool)])-> Board -> Colour -> GameTree
buildAlphaBetaTree generate board colour =
    let moves = generate board colour in GameTree board colour (makeNextStates moves)
  where
    makeNextStates :: [(Turn, Bool)] -> [((Turn, Bool), GameTree)]
    makeNextStates = map (\t -> (t, buildAlphaBetaTree generate (makeMove colour t board) (other colour)))

--3.----------------------------------------------------------
-- generate all the possible moves for a colour on a given board
generateAlphaBetaMoves ::  Board -> Colour -> [(Turn, Bool)]
generateAlphaBetaMoves  board colour = allMovesOf colour board

--4.----------------------------------------------------------
--remove the wanted information from the getTopAlphaBetaMove function
getBestAlphaBetaMove :: Int-> GameTree -> (Turn, Bool)
getBestAlphaBetaMove depth gameTree = fst $ getTopAlphaBetaMove depth (gameTurn gameTree) (nextMoves gameTree)


--5.----------------------------------------------------------
--read the max move from first layer of children
getTopAlphaBetaMove :: Int -> Colour -> [((Turn, Bool), GameTree)] -> ((Turn, Bool), Int)
getTopAlphaBetaMove _ _ [p] = (fst p, 0)
getTopAlphaBetaMove depth colour gameTree = maximumBy (compare `on` snd) choices
                                  where choices = zip (map fst gameTree) $ map (\turn -> alphaBetaPruning (depth-1) colour False turn (minBound, maxBound)) $ (map snd gameTree)

--6.----------------------------------------------------------
alphaBetaPruning ::  Int -> Colour -> Bool -> GameTree -> (Int, Int) -> Int
-- no available moves from this part
alphaBetaPruning _ aiColour _ (GameTree board _ []) _ = evaluateBoard aiColour board
-- reached search depth
alphaBetaPruning 0 aiColour _ (GameTree board _ _) _ = evaluateBoard aiColour board
-- evaluate a node
alphaBetaPruning depth aiColour True (GameTree board _ [next]) (alpha, beta) = alphaBetaPruning (depth-1) aiColour False (snd next) (alpha,beta)
alphaBetaPruning depth aiColour True (GameTree board col (next:moves)) (alpha, beta) =
   if nextAlpha >= beta then nextAlpha else
      max value $ alphaBetaPruning depth aiColour True (GameTree board col moves) (nextAlpha,beta)
    where value = alphaBetaPruning (depth-1) aiColour False (snd next) (alpha,beta)
          nextAlpha = max value alpha

alphaBetaPruning depth aiColour False (GameTree board _ [next]) (alpha, beta) = alphaBetaPruning (depth-1) aiColour True (snd next) (alpha,beta)
alphaBetaPruning depth aiColour False (GameTree board col (next:moves)) (alpha, beta) =
   if alpha >= nextBeta then nextBeta else
      min value $ alphaBetaPruning depth aiColour False (GameTree board col moves) (alpha,nextBeta)
    where value = alphaBetaPruning (depth-1) aiColour True (snd next) (alpha,beta)
          nextBeta = min value beta
