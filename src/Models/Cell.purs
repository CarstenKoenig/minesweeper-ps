module Models.Cell (
    Cell,
    Value(..),
    State(..),
    create, createMine, createEmpty,
    reveal, toggleFlag
) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Models.Coord (Coord)


type Cell =
    { coord :: Coord
    , value :: Value
    , state :: State
    }


data Value
    = ContainsMine
    | Empty Int -- : the number is the mine-count in the neighbourhood

derive instance genericValue :: Generic Value _
instance showValue :: Show Value where
    show = genericShow
instance eqValue :: Eq Value where
    eq = genericEq


data State
    = Hidden
    | Revealed
    | Flagged

derive instance genericState :: Generic State _
instance showState :: Show State where
    show = genericShow
instance eqState :: Eq State where
    eq = genericEq


create :: Coord -> Value -> State -> Cell
create coord value state =
    { coord, value, state }


createMine :: Coord -> Cell
createMine coord = 
    create coord ContainsMine Hidden


createEmpty :: Coord -> Int -> Cell
createEmpty coord nrSurroundingMines = 
    create coord (Empty nrSurroundingMines) Hidden

reveal :: Cell -> Cell
reveal cell =
    cell { state = Revealed }

toggleFlag :: Cell -> Cell
toggleFlag cell =
    cell { state = 
        case cell.state of
            Hidden -> Flagged
            Flagged -> Hidden
            Revealed -> Revealed
     }