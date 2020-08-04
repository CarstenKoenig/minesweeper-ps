module Components.Cell where
  
import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..), Component, liftEffect)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Models.Cell (Cell)
import Models.Cell as Cell
import Models.Coord (Coord)
import Web.Event.Event (EventType(..), preventDefault)
import Web.UIEvent.MouseEvent as ME

type Query = Const Void
type Input = Cell
data Output 
  = Reveal Coord
  | ToggleFlag Coord


cell :: forall m. MonadEffect m => Component HTML Query Input Output m
cell = Hooks.component createHook
  where
  createHook { outputToken } cellState = Hooks.pure cellDiv
    where 
    cellDiv =
      HH.div
          [ HP.classes 
              [ ClassName "Cell"
              , stateClass
              ] 
          , HE.onClick onClickHandler
          , HE.onMouseDown onMouseDownHandler
          , HE.handler (EventType "contextmenu") onContextMenueHandler
          ]
          content
      where
      content =
        case cellState.state of
          Cell.Hidden -> []
          Cell.Flagged -> flag
          Cell.Revealed ->
            case cellState.value of
              Cell.ContainsMine -> bomb
              Cell.Empty 0 -> []
              Cell.Empty n -> mineCountDisplay n
    onClickHandler _ =
      -- only reveal a Cell if it's Hidden
      if cellState.state /= Cell.Hidden then
        Nothing
      else
        Just $ Hooks.raise outputToken (Reveal cellState.coord)
    onMouseDownHandler mouseEvent =
      if ME.button mouseEvent == 2 then Just do
        liftEffect $ preventDefault $ ME.toEvent mouseEvent
        Hooks.raise outputToken (ToggleFlag cellState.coord)
      else
        Nothing
    onContextMenueHandler event =
      -- disable context-menu - we want to flag with the alternate mouse-button
      Just $ liftEffect $ preventDefault event
    stateClass =
      if cellState.state == Cell.Revealed && cellState.value == Cell.ContainsMine then
        ClassName "revealed mine"
      else if cellState.state == Cell.Revealed then
        ClassName "revealed"
      else
        ClassName "hidden"

  bomb =
    [ HH.span
      [ HP.class_ (ClassName "Bomb") ]
      [HH.text "💣"]
    ]

  flag =
    [ HH.span
      []
      [HH.text "🚩"]
    ]

  mineCountDisplay n =
    [ HH.span
      [ HP.class_ (ClassName $ "value-" <> show n)]
      [ HH.text (show n) ]
    ]