module Stardust.IO where

import Data.Int.Bits
import Prelude

import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

data Port m = Port (m Int) (Int -> m Unit)

logger :: forall m. MonadEffect m => Port m
logger = Port (log "Read" *> pure 0) (const $ log "Write")