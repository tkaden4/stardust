module Stardust.Debug where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT)
import Control.MonadPlus (class Alt, class Plus)
import Control.Plus (empty)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Stardust.IO (logger)
import Stardust.Instructions (Instruction(..))
import Stardust.VM (Evaluator)

newtype VMState = VMState {}

instance vmStateSemigroup :: Semigroup VMState where
  append (VMState s) (VMState k) = VMState {}

instance vmStateMonoid :: Monoid VMState where
  mempty = VMState {}

evaluator :: Evaluator (StateT VMState Effect)
evaluator = {
  next: pure $ Get 0xEEEE 0,
  ioInterrupts: pure $ Nothing,
  ports: Map.singleton 0xEEEE logger,
  registers: {
    register: \r -> pure 0,
    setRegister: \r _ -> pure unit
  },
  memory: {
    write: \sz _ _ -> do
      pure unit,
    read: \sz _ -> pure 0
  }
}