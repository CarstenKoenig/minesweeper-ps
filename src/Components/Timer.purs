module Components.Timer where
  
import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Timer (clearInterval, setInterval)
import Halogen (ClassName(..), Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.Query.EventSource (EventSource, Finalizer(..), effectEventSource, emit)
import Utils.String (padWithZeroes)

data Query a 
  = IsEnabled (Boolean -> a)
  | SetEnabled Boolean a
  | Reset a

type Input = Unit
type Output = Void

timer :: forall m. MonadAff m => Component HTML Query Input Output m
timer = Hooks.component \{ queryToken } _ -> Hooks.do

  seconds /\ secondsId <- Hooks.useState 0
  enabled /\ enabledId <- Hooks.useState false

  Hooks.useLifecycleEffect do
    subscriptionId <- subscribeToInterval enabledId (Hooks.modify_ secondsId)
    pure $ Just $ Hooks.unsubscribe subscriptionId

  Hooks.useQuery queryToken case _ of
    IsEnabled reply -> do
      pure (Just $ reply enabled)
    SetEnabled enable cont -> do
      Hooks.modify_ enabledId (const enable)
      pure $ Just cont
    Reset cont -> do
      Hooks.modify_ enabledId (const false)
      Hooks.modify_ secondsId (const 0)
      pure $ Just cont

  Hooks.pure do
    HH.div
      [ HP.class_ (ClassName "Timer") ]
      [ HH.p_ 
        [ HH.text $ padWithZeroes 3 $ show seconds ]
      ]

  where
  -- subscribes to a one-second timer, updating the state on every tick
  subscribeToInterval enabledId modifySeconds =
    Hooks.subscribe (intervalES 1000 addSecond)
    where 
    addSecond = do
      isEnabled <- Hooks.get enabledId
      if isEnabled then
        modifySeconds (_ + 1)
      else
        pure unit


-- a very simple event-source emitting a unit-action after every 'interval_ms'
intervalES :: forall m. MonadAff m => Int -> HookM m Unit ->  EventSource m (HookM m Unit)
intervalES interval_ms action =
  effectEventSource \emitter -> do
    intervalId <- setInterval interval_ms (emit emitter action)
    pure $ Finalizer (clearInterval intervalId)
