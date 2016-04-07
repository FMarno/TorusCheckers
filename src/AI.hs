module AI where

import TorusCheckers

data GameTree = GameTree {
                    gameBoard :: Board,
                    gameTurn :: Colour,
                    nextMoves :: [(Position, GameTree)]
                    }
    deriving (Show)
