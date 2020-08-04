module Components.Game where
  
import Prelude

import Components.Cell (cell)
import Components.Cell as Cell
import Components.Timer (timer)
import Components.Timer as Timer
import Data.Const (Const)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (AttrName(..), ClassName(..), Component, Slot)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Models.Board as Board
import Models.Coord (Coord)
import Models.Game (Game)
import Models.Game as Game
import Utils.String (padWithZeroes)

type Query = Const Void
type Input = Game
type Output = Void

-- Slots for Child-Components
type Slots = 
    -- we have slots for Cell-componentes, parameters are the query and input of
    -- the Cell-componenent and the slot-addresses/keys are the cells coords
    ( cells :: Slot Cell.Query Cell.Input Coord

    -- there is also a slot for a timer-component
    -- there will be only one so the address is just unit
    , timer :: Slot Timer.Query Timer.Input Unit
    )

-- Proxy to help bridge value/type levels and address for slots
_cells = SProxy :: SProxy "cells"
_timer = SProxy :: SProxy "timer"

-- the game component
-- child-components need to be able to lift effects so MonadEffect is neccessary
game :: forall m. MonadAff m => MonadEffect m => Component HTML Query Input Output m
game = Hooks.component createComponent
  where
  createComponent { slotToken } initialGameState = Hooks.do
    -- setup hooks
    -- this component uses State of type Models.Game
    gameState /\ gameStateId <- Hooks.useState initialGameState

    -- render component
    Hooks.pure $ view gameState gameStateId
    where
    view gameState@{rows, cols, board} gameStateId =
        HH.div
            [ HP.class_ (ClassName "Game") ]
            [ HH.div
                [ HP.class_ (ClassName "Header") ]
                [ mineCount
                , resetButton
                , HH.slot _timer unit timer unit (const Nothing) 
                ]
            , HH.div
                [ HP.class_ (ClassName "Main") 
                , HP.attr 
                    (AttrName "style") 
                    ("grid-template-columns: repeat(" <> show cols <> ", 1fr); " <> "grid-template-rows: repeat(" <> show rows <> ", 1fr);")
                ]
                (foldMap identity
                    [ Board.toArray renderCell board
                    , [ wonOverlay, lostOverlay ]
                    ]
                )
            ]
        where
        mineCount =
            HH.div
                [ HP.class_ (ClassName "MineCount") ]
                [ HH.p_ 
                    [ HH.text $ padWithZeroes 3 $ show minesLeft ]
                ]
            where
            minesLeft = 
                max 0 $
                Board.numberOfBombs board - Board.numberOfFlaggedCells board

        resetButton =
            HH.div
                [ HP.class_ (ClassName "Reset") ]
                [ HH.button 
                    [ HE.onClick \_ -> Just onReset ]
                    [ HH.text "reset" ]
                ]

        wonOverlay =
            HH.div
                [ HP.classes classes ]
                [ HH.h1_ [ HH.text "congrats - you won!" ] ]
            where
            classes =
                if Game.isWon gameState then
                    [ ClassName "WonOverlay" ]
                else
                    [ ClassName "WonOverlay"
                    , ClassName "hidden"
                    ]

        lostOverlay =
            HH.div
                [ HP.classes classes ]
                [ HH.h1_ [ HH.text "sorry you hit a mine" ] ]
            where
            classes =
                if Game.isLost gameState then
                    [ ClassName "LostOverlay" ]
                else
                    [ ClassName "LostOverlay"
                    , ClassName "hidden"
                    ]

        -- renders a cell into a child-component slot addressed by the cells' coord
        -- react to output from the cell-component with the onCellClicked-handler 
        -- (it's really only clicks)
        renderCell cellState =
            HH.slot _cells cellState.coord cell cellState onCellClicked

        -- don't react to clicks when the game is over
        onCellClicked _ | Game.isGameOver gameState = Nothing
        -- react to a click on a cell (passed up by the cell-component)
        onCellClicked action = Just $ do
            newState <- Hooks.modify gameStateId modification
            enableTimer (not $ Game.isGameOver newState)
            where
            modification = case action of
                Cell.ToggleFlag coord -> Game.toggleFlag coord
                Cell.Reveal coord     -> Game.revealCell coord

        onReset :: HookM m Unit
        onReset = do
            resetTimer
            oldGame <- Hooks.get gameStateId
            newGame <- liftEffect $ Game.reset oldGame
            Hooks.put gameStateId newGame

    enableTimer enable = do
        void $ Hooks.query slotToken _timer unit $ H.tell (Timer.SetEnabled enable)

    resetTimer = do
        void $ Hooks.query slotToken _timer unit $ H.tell Timer.Reset