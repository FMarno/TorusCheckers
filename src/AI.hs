module AI where

import           Data.Function
import           Data.List
import           Data.Ord
import           Debug.Trace
import           TorusCheckers

data GameTree = GameTree {
                    gameBoard :: Board,
                    gameTurn  :: Colour,
                    nextMoves :: [((Turn, Bool), GameTree)]
                    }
    deriving (Show)

--1.----------------------------------------------------------
makeAlphaBetaPruningMove :: Int -> State -> State
makeAlphaBetaPruningMove depth (State b@(Board size ps play) turn)
   = State newBoard (other turn)
    where newBoard = makeMove turn bestMove b
          bestMove = getBestAlphaBetaMove depth $ buildAlphaBetaTree generateAlphaBetaMoves b turn

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
getBestAlphaBetaMove :: Int-> GameTree -> (Turn, Bool)
getBestAlphaBetaMove depth gameTree = choice --trace (show (gameTurn gameTree) ++ ":" ++ show choice) choice
          where getTurn (t,tree) = t
                choice = getTurn $ getTopAlphaBetaMove depth (gameTurn gameTree) (nextMoves gameTree)


--5.----------------------------------------------------------

getTopAlphaBetaMove :: Int -> Colour -> [((Turn, Bool), GameTree)] -> ((Turn, Bool), GameTree)
getTopAlphaBetaMove _ _ [p] = p
getTopAlphaBetaMove depth colour pieces = choice --trace (show $ zip (map selectFirst values) (map fst pieces)) choice
-- start with false because its applied to all the first children
   where values = map (\(_, tree) -> alphaBetaPruning  depth colour False tree (minBound, maxBound)) pieces
   -- find the value of the node with the highest alpha value
         choice = fst $ maximumBy (compare `on` snd) $ zip pieces values

--6.----------------------------------------------------------
--TODO here Down
alphaBetaPruning ::  Int -> Colour -> Bool -> GameTree -> (Int, Int) -> (Int,Int,Int)
alphaBetaPruning  _ colour _ (GameTree board _ []) (alpha, beta) = (evaluateBoard colour board, alpha, beta)
alphaBetaPruning  0 colour _ (GameTree board _ _) (alpha, beta) = (evaluateBoard colour board, alpha, beta)
alphaBetaPruning  depth colour maxPlayer (GameTree _ _ moves) (alpha, beta)
          = h
          where (treeOfMoves : allOtherPossibleMoves) = map treeOf moves
                treeOf (p,tree) = tree
                valueOfFirstOppSelectFirstntChild = alphaBetaPruning  (depth-1) colour (not maxPlayer) treeOfMoves (alpha,beta)
                h = foldl' (accumulate  depth colour maxPlayer) valueOfFirstOppSelectFirstntChild allOtherPossibleMoves

accumulate ::  Int-> Colour -> Bool -> (Int, Int, Int) -> GameTree -> (Int, Int, Int)
accumulate  depth colour maxPlayer input@(value, alpha, beta) gameTree
   | beta <= alpha = input
   | otherwise     = (v, newalpha, newbeta)
      where operator
              | maxPlayer = max
              | otherwise = min
            v = operator value newh
            (newh, newa, newb) = alphaBetaPruning (depth-1) colour (not maxPlayer) gameTree (alpha, beta)
            newalpha
              | maxPlayer = operator alpha v
              | otherwise = alpha
            newbeta
              | maxPlayer = beta
              | otherwise = operator beta v

selectFirst :: (Int, Int,Int) -> Int
selectFirst (a,b,c) = a

--------------------
-- | checkWon board == colour
--   = (100 , alpha, beta)
-- | checkWon board == other colour
--   = (-100, alpha, beta)
-- | otherwise
