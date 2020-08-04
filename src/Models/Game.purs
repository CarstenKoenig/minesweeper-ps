module Models.Game (
    Game,
    initialize,
    reset,
    revealCell, toggleFlag,
    isGameOver, isWon, isLost
) where

import Prelude

import Effect (Effect)
import Models.Board (Board)
import Models.Board as Board
import Models.Coord (Coord)

type Game =
    { rows :: Int
    , cols :: Int
    , board :: Board
    }

initialize :: Int -> Int -> Int -> Effect Game
initialize rows cols nrMines = do
    board <- Board.initialize rows cols nrMines
    pure { rows, cols, board }


reset :: Game -> Effect Game
reset game = do
    newBoard <- Board.initialize game.rows game.cols (Board.numberOfBombs game.board)
    pure game{ board = newBoard }


revealCell :: Coord -> Game -> Game
revealCell coord game | not (isGameOver game) =
    modifyBoard (Board.revealCell coord) game
revealCell _ game = game


toggleFlag :: Coord -> Game -> Game
toggleFlag coord game | not (isGameOver game) = 
    modifyBoard (Board.toggleFlag coord) game
toggleFlag _ game = game


isGameOver :: Game -> Boolean
isGameOver game =
    Board.isFinished game.board

isWon :: Game -> Boolean
isWon game =
    Board.isWonState game.board

isLost :: Game -> Boolean
isLost game =
    Board.isLostState game.board

modifyBoard :: (Board -> Board) -> Game -> Game
modifyBoard modify game =
    game { board = modify game.board }