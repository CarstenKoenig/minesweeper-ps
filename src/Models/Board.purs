module Models.Board (
    Board,
    initialize,
    isFinished, isWonState, isLostState,
    numberOfBombs, numberOfCells,
    numberOfFlaggedCells, numberOfReveledBombs, numberOfReveledCells,
    toArray,
    toUnfoldable,
    revealCell, toggleFlag
) where

import Prelude

import Algorithms.Closure (closure)
import Data.Array (concatMap, filter, foldr, length, (!!), (..))
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Unfoldable (class Unfoldable)
import Effect (Effect)
import Models.Cell (Cell)
import Models.Cell as Cell
import Models.Coord (Coord)
import Models.Coord as Coord

data Board = Board (Array (Array Cell))

initialize :: Int -> Int -> Int -> Effect Board
initialize rows cols nrMines = do
    mineCoords <- Coord.randomCoords rows cols nrMines
    pure $ Board $ cells mineCoords
    where
    cells mineCoords = do
        row <- 0..(rows-1)
        pure $ do
            col <- 0..(cols-1)
            let coord = Coord.create row col
            pure $
                if Set.member coord mineCoords then
                    Cell.createMine coord
                else
                    Cell.createEmpty coord (surroundingMineCount mineCoords coord)
    surroundingMineCount mineCoords coord =
        Coord.neighbours coord
        # Set.intersection mineCoords
        # Set.size


isFinished :: Board -> Boolean
isFinished board =
  isLostState board || isWonState board


isLostState :: Board -> Boolean
isLostState board =
  numberOfReveledBombs board > 0


isWonState :: Board -> Boolean
isWonState board =
  numberOfCells board == numberOfReveledCells board + numberOfFlaggedCells board


numberOfCells :: Board -> Int
numberOfCells (Board cells) =
  cells
  # concatMap identity
  # length


numberOfFlaggedCells :: Board -> Int
numberOfFlaggedCells (Board cells) =
  cells
  # concatMap identity
  # filter (\cell -> cell.state == Cell.Flagged)
  # length


numberOfBombs :: Board -> Int
numberOfBombs (Board cells) =
  cells
  # concatMap identity
  # filter (\cell -> cell.value == Cell.ContainsMine)
  # length


numberOfReveledBombs :: Board -> Int
numberOfReveledBombs (Board cells) =
  cells
  # concatMap identity
  # filter (\cell -> cell.state == Cell.Revealed && cell.value == Cell.ContainsMine)
  # length


numberOfReveledCells :: Board -> Int
numberOfReveledCells (Board cells) =
  cells
  # concatMap identity
  # filter (\cell -> cell.state == Cell.Revealed)
  # length


toArray :: forall a. (Cell -> a) -> Board -> Array a
toArray mapCell (Board cells) = do
  cellRow <- cells
  cell <- cellRow
  pure $ mapCell cell


toUnfoldable :: forall a f. Unfoldable f => (Cell -> a) -> Board -> f a
toUnfoldable mapCell = 
  Array.toUnfoldable <<< toArray mapCell


revealCell :: Coord -> Board -> Board
revealCell coord board =
    revealCells cellsToReveal board
    where
        cellsToReveal = closure more coord
        more coord' | isEmpty coord' = Coord.neighbours coord'
        more _ = Set.empty
        isEmpty coord' =
            case getCell coord' board of
                Just cell -> cell.value == Cell.Empty 0
                Nothing   -> false


toggleFlag :: Coord -> Board -> Board
toggleFlag coord =
    modifyCellAt coord Cell.toggleFlag


revealCells :: forall f. Foldable f => f Coord -> Board -> Board
revealCells coords board =
    foldr revealAt board coords


revealAt :: Coord -> Board -> Board
revealAt coord =
    modifyCellAt coord Cell.reveal


getCell :: Coord -> Board -> Maybe Cell
getCell { row, col } (Board cells) = do
    cellRow <- cells !! row
    cellRow !! col


modifyCellAt :: Coord -> (Cell -> Cell) -> Board -> Board
modifyCellAt { row, col } modify (Board cells) =
    Board newCells
    where
        newCells =
            fromMaybe cells $
            Array.modifyAt 
                row 
                (\cellRow -> fromMaybe cellRow $ Array.modifyAt col modify cellRow)
                cells