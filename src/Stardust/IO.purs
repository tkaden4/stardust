module Stardust.IO where

import Data.Int.Bits
import Prelude

import Control.Monad.State (class MonadState, get, modify_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff (Aff)
import Stardust.VM (class VM, Interrupt(..))

data Device m = Device Int (Int -> m Unit) (m Int) (m Unit)