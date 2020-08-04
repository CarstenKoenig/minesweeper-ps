module Utils.EventSource where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Effect.Timer (clearInterval, setInterval)
import Halogen.Query.EventSource (EventSource, Finalizer(..), effectEventSource, emit)


-- a very simple event-source emitting a unit after every 'interval_ms'
interval :: forall m. MonadAff m => Int -> EventSource m Unit
interval interval_ms =
  effectEventSource \emitter -> do
    intervalId <- setInterval interval_ms (emit emitter unit)
    pure $ Finalizer (clearInterval intervalId)